OBJ := $(addprefix target/, $(patsubst %.c, %.o, $(wildcard *.c)))
CFLAGS  :=  -pedantic -Wall -W -Werror -fPIC
LDFLAGS :=  -L.

.PHONY: all clean

all: target/libnotify.a target/libnotify.so target/lua_libnotify.so

target/%.o: %.c | target
	$(COMPILE.c) $(OUTPUT_OPTION) $<

target/libnotify.a: target/libnotify.o
	$(AR) $(ARFLAGS) $@ $^

target/libnotify.so: target/libnotify.o
	$(CC) -shared $^ -o $@

target/lua_libnotify.so: target/libnotify.o target/lua.o
	$(CC) -shared -llua5.4 $^ -o $@

target:
	mkdir -p $@

clean:
	-rm -rf target
