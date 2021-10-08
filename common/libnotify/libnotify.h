#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void notify(const char* msg);
void notify_with_pid(const char* msg);

#ifdef __cplusplus
}
#endif
