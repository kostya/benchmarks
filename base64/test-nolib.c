#include <libnotify.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

typedef unsigned int uint;
const char* chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static char decode_table[256];

int encode_size(int size) {
  return (int)(size * 4 / 3.0) + 6;
}

int decode_size(int size) {
  return (int)(size * 3 / 4.0) + 6;
}

void init_decode_table() {
  for (int i = 0; i < 256; i++) {
    char ch = (char)i;
    char code = -1;
    if (ch >= 'A' && ch <= 'Z') code = ch - 0x41;
    if (ch >= 'a' && ch <= 'z') code = ch - 0x47;
    if (ch >= '0' && ch <= '9') code = ch + 0x04;
    if (ch == '+' || ch == '-') code = 0x3E;
    if (ch == '/' || ch == '_') code = 0x3F;
    decode_table[i] = code;
  }
}

#define next_char(x) char x = decode_table[(unsigned char)*str++]; if (x < 0) return 1;

int decode(int size, const char* str, size_t* out_size, char* output) {
  char *out = output;
  while (size > 0 && (str[size - 1] == '\n' || str[size - 1] == '\r' || str[size - 1] == '=')) size--;
  const char* ends = str + size - 4;
  while (1) {
    if (str > ends) break;
    while (*str == '\n' || *str == '\r') str++;

    if (str > ends) break;
    next_char(a); next_char(b); next_char(c); next_char(d);

    *out++ = (char)(a << 2 | b >> 4);
    *out++ = (char)(b << 4 | c >> 2);
    *out++ = (char)(c << 6 | d >> 0);
  }

  int mod = (ends - str + 4) % 4;
  if (mod == 2) {
    next_char(a); next_char(b);
    *out++ = (char)(a << 2 | b >> 4);
  } else if (mod == 3) {
    next_char(a); next_char(b); next_char(c);
    *out++ = (char)(a << 2 | b >> 4);
    *out++ = (char)(b << 4 | c >> 2);
  }

  *out = '\0';
  *out_size = out - output;
  return 0;
}

void encode(int size, const char* str, size_t* out_size, char* output) {
  char *out = output;
  const char* ends = str + (size - size % 3);
  uint n;
  while (str != ends) {
    uint32_t n = __builtin_bswap32(*(uint32_t*)str);
    *out++ = chars[(n >> 26) & 63];
    *out++ = chars[(n >> 20) & 63];
    *out++ = chars[(n >> 14) & 63];
    *out++ = chars[(n >> 8) & 63];
    str += 3;
  }
  int pd = size % 3;
  if  (pd == 1) {
    n = (uint)*str << 16;
    *out++ = chars[(n >> 18) & 63];
    *out++ = chars[(n >> 12) & 63];
    *out++ = '=';
    *out++ = '=';
  } else if (pd == 2) {
    n = (uint)*str++ << 16;
    n |= (uint)*str << 8;
    *out++ = chars[(n >> 18) & 63];
    *out++ = chars[(n >> 12) & 63];
    *out++ = chars[(n >> 6) & 63];
    *out++ = '=';
  }
  *out = '\0';
  *out_size = out - output;
}

size_t b64_encode(char *dst, const char* src, size_t src_size) {
  size_t encoded_size;
  encode(src_size, src, &encoded_size, dst);
  return encoded_size;
}

size_t b64_decode(char *dst, const char* src, size_t src_size) {
  size_t decoded_size;
  if (decode(src_size, src, &decoded_size, dst) != 0) {
    fputs("error when decoding", stderr);
    exit(EXIT_FAILURE);
  }
  return decoded_size;
}

int main() {
  init_decode_table();

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
  size_t len = snprintf(msg, sizeof(msg), "C/gcc\t%d", getpid());
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
