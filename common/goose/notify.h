/* common/libnotify for the Goose tests, which reach C through `extern fn`
   and `goose --include notify.h`. The generated C defines sl_u8, the C shape
   of a Goose slice of bytes, before it includes this. */

#include <stdio.h>

#include <libnotify.h>

#ifdef __clang__
#define GOOSE_COMPILER "clang"
#else
#define GOOSE_COMPILER "gcc"
#endif

/* "Goose/<C compiler><variant>", as the C tests name theirs ("C/clang"). */
void goose_notify_start(sl_u8 variant) {
  char name[48];
  snprintf(name, sizeof(name), "Goose/%s%.*s", GOOSE_COMPILER, (int)variant.len,
           (const char *)variant.data);
  notify_with_pid(name);
}

void goose_notify_stop(void) { notify("stop"); }
