#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void notify(const char *msg);
void notify_with_pid(const char *msg);

#ifdef __cplusplus
}

#include <array>
#include <sstream>

template <size_t count, typename arg_type, typename... args>
inline void format_arguments(std::array<std::string, count> &formatted_strings,
                             arg_type &&argument, args &&...arguments) {
  constexpr auto args_size = sizeof...(args);
  std::ostringstream stream;
  stream << argument;
  formatted_strings[count - 1 - args_size] = stream.str();
  if constexpr (args_size > 0) {
    format_arguments(formatted_strings, std::forward<args>(arguments)...);
  }
}

template <typename... args>
std::string format(const std::string_view &format_string, args &&...arguments) {
  std::array<std::string, sizeof...(args)> formatted_args;
  format_arguments(formatted_args, std::forward<args>(arguments)...);

  std::ostringstream stream;
  auto fmt_counter{0};
  auto formatted_args_iter{formatted_args.cbegin()};
  auto formatted_args_end{formatted_args.cend()};
  for (const auto &ch : format_string) {
    // escape seq and inner replacement field are ignored for brevity
    if (ch == '{') {
      fmt_counter++;
    } else if (ch == '}') {
      // ignore all extra formatters for brevity
      if (--fmt_counter == 0 && formatted_args_iter != formatted_args_end) {
        stream << *formatted_args_iter++;
      }
    } else if (fmt_counter == 0) {
      stream << ch;
    }
  }

  return stream.str();
}

struct notify_stop_on_exit_t {
  ~notify_stop_on_exit_t() { notify("stop"); }
};

template <typename... args>
auto notifying_invoke(auto &&fn, const std::string_view &fmt,
                      args &&...arguments) {
  // TODO: use std::format when available for both GCC and Clang
  const auto &formatted = format(fmt, std::forward<args>(arguments)...);
  notify_with_pid(formatted.c_str());

  notify_stop_on_exit_t notify_stop;
  return fn();
}

#endif
