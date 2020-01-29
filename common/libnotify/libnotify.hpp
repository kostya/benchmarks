#pragma once

#include <string>

extern "C" void notify(const char* msg, size_t len);

void notify(const std::string& msg) {
  notify(msg.c_str(), msg.length());
}
