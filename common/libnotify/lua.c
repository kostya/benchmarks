#include "lua5.4/lua.h"
#include "lua5.4/lauxlib.h"
#include "libnotify.h"

static int lua_notify(lua_State* L) {
  notify(luaL_checklstring(L, 1, NULL));
  return 1;
}

static int lua_notify_with_pid(lua_State* L) {
  notify_with_pid(luaL_checklstring(L, 1, NULL));
  return 1;
}

static const luaL_Reg libnotify[] = {
  {"notify", lua_notify},
  {"notify_with_pid", lua_notify_with_pid},
  {NULL, NULL}
};

LUAMOD_API int luaopen_lua_libnotify(lua_State *L) {
  luaL_newlib(L, libnotify);
  return 1;
}
