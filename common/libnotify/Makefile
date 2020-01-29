NAME    :=  libnotify
SNAME   :=  $(NAME).a
DNAME   :=  $(NAME).so
SRC     :=  $(wildcard *.c)
OBJ     :=  $(SRC:.c=.o)
CFLAGS  :=  -pedantic -Wall -W -Werror -fPIC
LDFLAGS :=  -L.

.PHONY: all clean fclean re

all: $(SNAME) $(DNAME)

$(SNAME): $(OBJ)
	$(AR) $(ARFLAGS) $@ $^

$(DNAME): LDFLAGS += -shared
$(DNAME): $(OBJ)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	$(RM) $(OBJ)

fclean: clean
	$(RM) $(SNAME) $(DNAME)

re: fclean all
