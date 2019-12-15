#include "stdlib.h"
#include "stdio.h"
#include <string.h>
#include <libsocket/libinetsocket.h>
#include "time.h"
#include "libbase64.h"
#include "../lib/config.h"

int encode_size(int size) {
  return (int)(size * 4 / 3.0) + 6;
}

int decode_size(int size) {
  return (int)(size * 3 / 4.0) + 6;
}

int main() {
  const int STR_SIZE = 131072;
  const int TRIES = 8192;

  char str[STR_SIZE + 1];
  memset(str, 'a', STR_SIZE);
  str[STR_SIZE] = '\0';

  int sock = create_inet_stream_socket("localhost", "9001", LIBSOCKET_IPv4, 0);
  if (sock != -1) {
    char msg[] = "C aklomp";
    send(sock, msg, sizeof(msg), 0);
    destroy_inet_socket(sock);
  }

  size_t str2_size;
  char str2[encode_size(STR_SIZE)];
  str2[0] = 0;
  base64_encode(str, STR_SIZE, str2, &str2_size, 0);
  printf("encode %s... to %s...: ", strndup(str, 4), strndup(str2, 4));

  int s = 0;
  clock_t t = clock();
  for (int i = 0; i < TRIES; i++) {
    size_t str2_size;
    char str2[encode_size(STR_SIZE)];
    base64_encode(str, STR_SIZE, str2, &str2_size, 0);
    s += str2_size;
  }
  printf("%d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);

  size_t str3_size;
  char str3[decode_size(str2_size)];
  base64_decode(str2, str2_size, str3, &str3_size, 0);
  printf("decode %s... to %s...: ", strndup(str2, 4), strndup(str3, 4));

  s = 0;
  t = clock();
  for (int i = 0; i < TRIES; i++) {
    size_t str3_size;
    char str3[decode_size(str2_size)];
    if (base64_decode(str2, str2_size, str3, &str3_size, 0) != 1) {
      printf("error when decoding");
    }
    s += str3_size;
  }
  printf("%d, %.2f\n", s, (float)(clock() - t)/CLOCKS_PER_SEC);
}
