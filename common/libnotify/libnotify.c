#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include "libnotify.h"

void notify(const char* msg, size_t len) {
  int sock = socket(AF_INET, SOCK_STREAM, 0);

  if (sock < 0) {
    return;
  }

  struct sockaddr_in serv_addr;
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(9001);
  inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr);

  if (!connect(sock, (struct sockaddr *)&serv_addr, sizeof(serv_addr))) {
    send(sock, msg, len, 0);
  }
  close(sock);
}
