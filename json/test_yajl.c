#include <libnotify.h>
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

void dump(char* buf, size_t size, const coordinate_t* coord) {
	snprintf(buf, size, "{%lf, %lf, %lf}", coord->x, coord->y, coord->z);
}

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

str_t readfile(const char* filename) {
	struct stat file_stat;
	int ret = 0;
	unsigned char* buf = NULL;

	int fd = open(filename, O_RDONLY);
	if (fd < 0) {
		fprintf(stderr, "Cannot open %s: %s\n", filename, strerror(errno));
		exit(EXIT_FAILURE);
	}

	ret = fstat(fd, &file_stat);
	if (ret < 0) {
		fprintf(stderr, "Cannot fstat %s: %s\n", filename, strerror(errno));
		exit(EXIT_FAILURE);
	}

	buf = calloc(file_stat.st_size, sizeof(unsigned char));
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

int verify(const char* buf, size_t size, const coordinate_t* expected) {
	context_t ctx = init_context();

	yajl_handle yajl = yajl_alloc(&ycallbacks, NULL, &ctx);

	yajl_parse(yajl, (const unsigned char*)buf, size);
	yajl_complete_parse(yajl);

	yajl_free(yajl);

	if (ctx.value.x != expected->x || ctx.value.y != expected->y || ctx.value.z != expected->z) {
		char str_x[50], str_y[50];

		dump(str_x, sizeof(str_x) / sizeof(str_x[0]), &ctx.value);
		dump(str_y, sizeof(str_y) / sizeof(str_y[0]), expected);

		fprintf(stderr, "%s != %s\n", str_x, str_y);

		return 1;
	}

	return 0;
}

int main() {
	const char test_buffer_1[] = "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}";
	const char test_buffer_2[] = "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}";

	const coordinate_t expected = {
		.x = 2.0,
		.y = 0.5,
		.z = 0.25
	};

	if (verify(test_buffer_1, sizeof(test_buffer_1) / sizeof(test_buffer_1[0]), &expected)) {
		exit(EXIT_FAILURE);
	}

	if (verify(test_buffer_2, sizeof(test_buffer_2) / sizeof(test_buffer_2[0]), &expected)) {
		exit(EXIT_FAILURE);
	}

	__attribute__((__cleanup__(str_free))) str_t str = readfile("/tmp/1.json");

	context_t ctx = init_context();
	yajl_handle yajl = yajl_alloc(&ycallbacks, NULL, &ctx);

	notify_with_pid("C/" COMPILER " (yajl)");

	yajl_parse(yajl, str.buf, str.size);
	yajl_complete_parse(yajl);

	notify("stop");

	yajl_free(yajl);

	char tmp[50];
	dump(tmp, sizeof(tmp) / sizeof(tmp[0]), &ctx.value);

	printf("result = %s\n", tmp);
}
