#include <arpa/inet.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "libnotify.h"

static const size_t MAX_LEN = 32;

void notify(const char* msg) {
  int sock = socket(AF_INET, SOCK_STREAM, 0);

  if (sock < 0) {
    return;
  }

  struct sockaddr_in serv_addr;
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(9001);
  inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr);

  if (!connect(sock, (struct sockaddr *)&serv_addr, sizeof(serv_addr))) {
    send(sock, msg, strnlen(msg, MAX_LEN), 0);
  }
  close(sock);
}

void notify_with_pid(const char* msg) {
  char text[MAX_LEN];
  if (snprintf(text, sizeof(text), "%s\t%d", msg, getpid()) > 0) {
    notify(text);
  }
}
