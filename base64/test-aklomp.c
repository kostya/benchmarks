#include "libbase64.h"
#include <libnotify.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

int encode_size(int size) {
  return (int)(size * 4 / 3.0) + 6;
}

int decode_size(int size) {
  return (int)(size * 3 / 4.0) + 6;
}

size_t b64_encode(char *dst, const char* src, size_t src_size) {
  size_t encoded_size;
  base64_encode(src, src_size, dst, &encoded_size, 0);
  return encoded_size;
}

size_t b64_decode(char *dst, const char* src, size_t src_size) {
  size_t decoded_size;
  if (base64_decode(src, src_size, dst, &decoded_size, 0) != 1) {
    fputs("error when decoding", stderr);
    exit(EXIT_FAILURE);
  }
  return decoded_size;
}

int main() {
  const char *fixtures[][2] =
    {{"hello", "aGVsbG8="}, {"world", "d29ybGQ="}};
  const int num_fixtures = sizeof(fixtures)/sizeof(fixtures[0]);
  for (int i = 0; i < num_fixtures; ++i) {
    const char *src = fixtures[i][0];
    size_t src_len = strlen(src);

    const char *dst = fixtures[i][1];
    size_t dst_len = strlen(dst);

    char encoded[encode_size(src_len)];
    size_t encoded_size = b64_encode(encoded, src, src_len);
    if (dst_len != encoded_size || strncmp(encoded, dst, encoded_size)) {
      char fmt[20];
      snprintf(fmt, sizeof(fmt), "%%.%lds != %%.%lds\n", encoded_size, dst_len);
      fprintf(stderr, fmt, encoded, dst);
      exit(EXIT_FAILURE);
    }

    char decoded[decode_size(dst_len)];
    size_t decoded_size = b64_decode(decoded, dst, dst_len);
    if (src_len != decoded_size || strncmp(decoded, src, decoded_size)) {
      char fmt[20];
      snprintf(fmt, sizeof(fmt), "%%.%lds != %%.%lds\n", decoded_size, src_len);
      fprintf(stderr, fmt, decoded, src);
      exit(EXIT_FAILURE);
    }
  }

  const int STR_SIZE = 131072;
  const int TRIES = 8192;

  char str[STR_SIZE];
  memset(str, 'a', STR_SIZE);
  char str2[encode_size(STR_SIZE)];
  size_t str2_size = b64_encode(str2, str, STR_SIZE);
  char str3[decode_size(str2_size)];
  b64_decode(str3, str2, str2_size);

  char msg[32];
  size_t len = snprintf(msg, sizeof(msg), "C/gcc (aklomp)\t%d", getpid());
  notify(msg, len);

  int s_encoded = 0;
  clock_t t = clock();
  for (int i = 0; i < TRIES; i++) {
    char str2[encode_size(STR_SIZE)];
    s_encoded += b64_encode(str2, str, STR_SIZE);
  }
  float t_encoded = (float)(clock() - t) / CLOCKS_PER_SEC;

  int s_decoded = 0;
  clock_t t1 = clock();
  for (int i = 0; i < TRIES; i++) {
    char str3[decode_size(str2_size)];
    s_decoded += b64_decode(str3, str2, str2_size);
  }
  float t_decoded = (float)(clock() - t1) / CLOCKS_PER_SEC;

  const char stop_msg[] = "stop";
  notify(stop_msg, sizeof(stop_msg));

  printf("encode %.4s... to %.4s...: %d, %.2f\n",
         str, str2, s_encoded, t_encoded);
  printf("decode %.4s... to %.4s...: %d, %.2f\n",
         str2, str3, s_decoded, t_decoded);
}
