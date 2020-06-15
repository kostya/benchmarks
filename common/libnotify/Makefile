NAME    :=  libnotify
SNAME   :=  target/$(NAME).a
DNAME   :=  target/$(NAME).so
OBJ := $(addprefix target/, $(patsubst %.c, %.o, $(wildcard *.c)))
CFLAGS  :=  -pedantic -Wall -W -Werror -fPIC
LDFLAGS :=  -L.

.PHONY: all clean

all: $(SNAME) $(DNAME)

target/%.o: %.c | target
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(SNAME): $(OBJ)
	$(AR) $(ARFLAGS) $@ $^

$(DNAME): LDFLAGS += -shared
$(DNAME): $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@

target:
	mkdir -p $@

clean:
	-rm -rf target
