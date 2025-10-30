#include <libnotify.h>
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <yyjson/yyjson.h>

#ifdef __clang__
#define COMPILER "clang"
#else
#define COMPILER "gcc"
#endif


struct coordinate_s {
  double x;
  double y;
  double z;
};

typedef struct coordinate_s coordinate_t;

struct str_s {
  char* buf;
  size_t size;
};

typedef struct str_s str_t;

void str_free(str_t* s) {
  free(s->buf);
}

str_t format(const char *fmt, ...) {
  va_list ap;

  // Determine required size.
  va_start(ap, fmt);
  int n = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);

  assert(n >= 0);

  size_t size = (size_t) n + 1; // One extra byte for '\0'
  char* p = malloc(size);
  assert(p != NULL);

  va_start(ap, fmt);
  n = vsnprintf(p, size, fmt, ap);
  va_end(ap);

  if (n < 0) {
    free(p);
    assert(false);
  }

  return (str_t){
    .buf = p,
    .size = size
  };
}

str_t dump(const coordinate_t* coord) {
  return format("{%g, %g, %g}", coord->x, coord->y, coord->z);
}

str_t readfile(const char* filename) {
  const int fd = open(filename, O_RDONLY);
  if (fd < 0) {
    fprintf(stderr, "Cannot open %s: %s\n", filename, strerror(errno));
    exit(EXIT_FAILURE);
  }

  struct stat file_stat;
  int ret = fstat(fd, &file_stat);
  if (ret < 0) {
    fprintf(stderr, "Cannot fstat %s: %s\n", filename, strerror(errno));
    exit(EXIT_FAILURE);
  }

  char* buf = malloc(file_stat.st_size);
  if (!buf) {
    fprintf(stderr, "Cannot allocate %ld bytes memory\n", file_stat.st_size);
    exit(EXIT_FAILURE);
  }

  ret = read(fd, buf, file_stat.st_size);
  if (ret != file_stat.st_size) {
    fprintf(stderr, "Cannot read %s: %s\n", filename, strerror(errno));
    exit(EXIT_FAILURE);
  }

  return (str_t){
    .buf = buf,
    .size = file_stat.st_size
  };
}

static void yyjson_doc_cleanup(yyjson_doc **doc) {
  yyjson_doc_free(*doc);
}

void doc_to_coordinates(yyjson_doc *doc, coordinate_t* result) {
  yyjson_val *root = yyjson_doc_get_root(doc);
  yyjson_val *coordinates = yyjson_obj_get(root, "coordinates");

  result->x = 0;
  result->y = 0;
  result->z = 0;

  size_t idx, max;
  yyjson_val *val;
  yyjson_arr_foreach(coordinates, idx, max, val) {
    yyjson_val *x = yyjson_obj_get(val, "x");
    yyjson_val *y = yyjson_obj_get(val, "y");
    yyjson_val *z = yyjson_obj_get(val, "z");

    result->x += yyjson_get_real(x);
    result->y += yyjson_get_real(y);
    result->z += yyjson_get_real(z);
  }

  result->x /= max;
  result->y /= max;
  result->z /= max;
}

void verify(const char* buf, const coordinate_t* expected) {
  coordinate_t actual;

  __attribute__((__cleanup__(yyjson_doc_cleanup))) yyjson_doc *doc = yyjson_read(buf, strlen(buf), 0);
  doc_to_coordinates(doc, &actual);

  if (actual.x != expected->x || actual.y != expected->y || actual.z != expected->z) {
    __attribute__((__cleanup__(str_free))) str_t str_x = dump(&actual);
    __attribute__((__cleanup__(str_free))) str_t str_y = dump(expected);
    fprintf(stderr, "%s != %s\n", str_x.buf, str_y.buf);
    exit(EXIT_FAILURE);
  }
}

int main() {
  const char test_buffer_1[] = "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}";
  const char test_buffer_2[] = "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}";

  const coordinate_t expected = {
    .x = 2.0,
    .y = 0.5,
    .z = 0.25
  };

  verify(test_buffer_1, &expected);
  verify(test_buffer_2, &expected);

  coordinate_t actual;

  __attribute__((__cleanup__(str_free))) str_t str = readfile("/tmp/1.json");

  notify_with_pid("C/" COMPILER " (yyjson)");

  __attribute__((__cleanup__(yyjson_doc_cleanup))) yyjson_doc *doc = yyjson_read(str.buf, str.size, 0);
  doc_to_coordinates(doc, &actual);

  notify("stop");

  __attribute__((__cleanup__(str_free))) str_t tmp = dump(&actual);
  printf("%s\n", tmp.buf);
}
