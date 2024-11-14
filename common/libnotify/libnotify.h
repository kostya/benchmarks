#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void notify(const char *msg);
void notify_with_pid(const char *msg);

#ifdef __cplusplus
}

#include <format>

struct notify_stop_on_exit_t {
  ~notify_stop_on_exit_t() { notify("stop"); }
};

template <typename... args>
auto notifying_invoke(auto &&fn, const std::string_view &fmt,
                      args &&...arguments) {
  const auto &formatted = std::vformat(fmt, std::make_format_args(arguments...));
  notify_with_pid(formatted.c_str());

  notify_stop_on_exit_t notify_stop;
  return fn();
}

#endif
