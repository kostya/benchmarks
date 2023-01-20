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

#include <yajl/yajl_parse.h>
#include <yajl/yajl_version.h>

#if (YAJL_MAJOR != 2)
#error "yajl 2.x is required"
#endif

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

struct context_s {
  size_t depth;
  size_t len;
  char item;
  bool in_coord;

  coordinate_t value;
};

typedef struct context_s context_t;

struct str_s {
  unsigned char* buf;
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
    .buf = (unsigned char*)p,
    .size = size
  };
}

void yajlptr_free(yajl_handle* yajl) {
  yajl_free(*yajl);
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

  unsigned char* buf = malloc(file_stat.st_size);
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

int cb_double(void* ctx_v, double value) {
  context_t* ctx = ctx_v;

  switch (ctx->item) {
  case 'x': ctx->value.x += value; break;
  case 'y': ctx->value.y += value; break;
  case 'z': ctx->value.z += value; break;
  default:;
  }

  return 1;
}

int cb_start_map(void* ctx_v) {
  context_t* ctx = ctx_v;

  ctx->depth++;

  if (ctx->depth == 2 && ctx->in_coord) {
    ctx->len++;
  }

  return 1;
}

int cb_map_key(void* ctx_v, const unsigned char* key, size_t length) {
  context_t* ctx = ctx_v;

  if (ctx->depth == 1) {
    ctx->in_coord = strncmp("coordinates", (const char*)key, length) == 0;
  } else if (ctx->depth == 2 && ctx->in_coord) {
    ctx->item = (length > 0 ? key[0] : '\0');
  }

  return 1;
}

int cb_end_map(void* ctx_v) {
  context_t* ctx = ctx_v;

  ctx->depth--;

  return 1;
}

int cb_end_array(void* ctx_v) {
  context_t* ctx = ctx_v;

  if (ctx->depth == 1 && ctx->in_coord) {
    ctx->value.x /= ctx->len;
    ctx->value.y /= ctx->len;
    ctx->value.z /= ctx->len;
  }

  return 1;
}

static yajl_callbacks ycallbacks = {
  .yajl_null = NULL,
  .yajl_boolean = NULL,
  .yajl_integer = NULL,
  .yajl_double = &cb_double,
  .yajl_number = NULL,
  .yajl_string = NULL,
  .yajl_start_map = &cb_start_map,
  .yajl_map_key = &cb_map_key,
  .yajl_end_map = &cb_end_map,
  .yajl_start_array = NULL,
  .yajl_end_array = &cb_end_array
};

context_t init_context() {
  return (context_t){
    .depth = 0,
    .len = 0,
    .item = '\0',
    .in_coord = false,
    .value = {
      .x = 0.0,
      .y = 0.0,
      .z = 0.0
    }
  };
}

void verify(const char* buf, size_t size, const coordinate_t* expected) {
  context_t ctx = init_context();

  __attribute__((__cleanup__(yajlptr_free))) yajl_handle yajl = yajl_alloc(&ycallbacks, NULL, &ctx);

  yajl_parse(yajl, (const unsigned char*)buf, size);
  yajl_complete_parse(yajl);

  if (ctx.value.x != expected->x || ctx.value.y != expected->y || ctx.value.z != expected->z) {
    __attribute__((__cleanup__(str_free))) str_t str_x = dump(&ctx.value);
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

  verify(test_buffer_1, sizeof(test_buffer_1) / sizeof(test_buffer_1[0]), &expected);
  verify(test_buffer_2, sizeof(test_buffer_2) / sizeof(test_buffer_2[0]), &expected);

  __attribute__((__cleanup__(str_free))) str_t str = readfile("/tmp/1.json");

  context_t ctx = init_context();
  __attribute__((__cleanup__(yajlptr_free))) yajl_handle yajl = yajl_alloc(&ycallbacks, NULL, &ctx);

  notify_with_pid("C/" COMPILER " (yajl)");

  yajl_parse(yajl, str.buf, str.size);
  yajl_complete_parse(yajl);

  notify("stop");

  __attribute__((__cleanup__(str_free))) str_t tmp = dump(&ctx.value);
  printf("%s\n", tmp.buf);
}
