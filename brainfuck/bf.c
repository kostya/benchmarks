#include <libnotify.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __clang__
# define COMPILER "clang"
#else
# define COMPILER "gcc"
#endif

struct op_list;

enum op_type {
  OP_INC,
  OP_MOVE,
  OP_LOOP,
  OP_PRINT
};

union op_value {
  int offset;
  struct op_list *list;
};

struct op {
  enum op_type type;
  union op_value value;
};

struct op_list {
  struct op *ops;
  int cap, len;
};

struct op op_new(enum op_type type, union op_value value)
{
  struct op op;

  op.type = type;
  if (type == OP_LOOP)
    op.value.list = value.list;
  else
    op.value.offset = value.offset;

  return op;
}

void op_list_free(struct op_list *list);

void op_free(struct op op)
{
  if (op.type == OP_LOOP) {
    op_list_free(op.value.list);
    free(op.value.list);
  }
}

void op_list_free(struct op_list *list)
{
  int i;

  for (i = 0; i < list->len; i += 1)
    op_free(list->ops[i]);

  free(list->ops);
}

void op_list_grow(struct op_list *list)
{
  if (list->ops == NULL) {
    list->cap = 4;
  } else {
    /* double the capacity */
    list->cap <<= 1;
  }
  list->ops = realloc(list->ops, sizeof(struct op) * list->cap);
}

int op_list_length(const struct op_list *list)
{
  return list->len;
}

struct op op_list_get(const struct op_list *list, int i)
{
  return list->ops[i];
}

void op_list_push(struct op_list *list, struct op op)
{
  if (list->len == list->cap)
    op_list_grow(list);

  list->ops[list->len++] = op;
}

struct string_iterator {
  const char *string;
  int pos;
};

char string_iterator_next(struct string_iterator *it)
{
  return it->string[it->pos++];
}

struct printer {
  int sum1;
  int sum2;
  bool quiet;
};

void print(struct printer *p, int n) {
  if (p->quiet) {
    p->sum1 = (p->sum1 + n) % 255;
    p->sum2 = (p->sum2 + p->sum1) % 255;
  } else {
    putc(n, stdout);
    fflush(stdout);
  }
}

int get_checksum(const struct printer *p) {
  return (p->sum2 << 8) | p->sum1;
}

void parse(struct string_iterator *it, struct op_list *ops)
{
  char c;
  struct op_list *loop_ops;
  union op_value value;
  enum op_type type;

  while ((c = string_iterator_next(it))) {
    switch (c) {
    case '+':
      type = OP_INC;
      value.offset = 1;
      break;
    case '-':
      type = OP_INC;
      value.offset = -1;
      break;
    case '>':
      type = OP_MOVE;
      value.offset = 1;
      break;
    case '<':
      type = OP_MOVE;
      value.offset = -1;
      break;
    case '.':
      type = OP_PRINT;
      value.offset = 0;
      break;
    case '[':
      loop_ops = calloc(1, sizeof(struct op_list));
      parse(it, loop_ops);

      type = OP_LOOP;
      value.list = loop_ops;
      break;
    case ']':
      return;
    default:
      continue;
    }

    op_list_push(ops, op_new(type, value));
  }
}

struct tape {
  int *tape;
  int cap, pos;
};

char tape_get(const struct tape tape)
{
  return tape.tape[tape.pos];
}

void tape_grow(struct tape *tape)
{
  int i, new_cap;

  new_cap = tape->cap << 1;
  tape->tape = realloc(tape->tape, sizeof(int) * new_cap);

  for (i = tape->cap; i < new_cap; i += 1)
    tape->tape[i] = 0;

  tape->cap = new_cap;
}

void tape_move(struct tape *tape, int amount)
{
  tape->pos += amount;

  if (tape->pos >= tape->cap)
    tape_grow(tape);
}

void tape_inc(struct tape tape, int amount)
{
  tape.tape[tape.pos] += amount;
}

void eval(const struct op_list *ops, struct tape *tape, struct printer *p) {
  int i, len;
  struct op op;

  for (i = 0, len = op_list_length(ops); i < len; i += 1) {
    switch ((op = op_list_get(ops, i)).type) {
    case OP_INC:
      tape_inc(*tape, op.value.offset);
      break;
    case OP_MOVE:
      tape_move(tape, op.value.offset);
      break;
    case OP_LOOP:
      while (tape_get(*tape) > 0)
        eval(op.value.list, tape, p);
      break;
    case OP_PRINT:
      print(p, tape_get(*tape));
      break;
    }
  }
}

void run(const char* code, struct printer *p) {
  struct tape tape = {.tape=calloc(1, sizeof(int)), .cap=1, .pos=0};
  struct op_list ops = {.ops=NULL, .cap=0, .len=0};
  struct string_iterator it = {.string=code, .pos=0};
  parse(&it, &ops);
  eval(&ops, &tape, p);
  free(tape.tape);
  op_list_free(&ops);
}

void verify() {
  char text[] = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>"
    "---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
  struct printer p_left = {.sum1=0, .sum2=0, .quiet=true};
  run(text, &p_left);
  int left = get_checksum(&p_left);

  struct printer p_right = {.sum1=0, .sum2=0, .quiet=true};
  char result[] = "Hello World!\n";
  size_t i = 0;
  while (result[i] != '\0') {
    print(&p_right, result[i++]);
  }
  int right = get_checksum(&p_right);
  if (left != right) {
    fprintf(stderr, "%d != %d", left, right);
    exit(EXIT_FAILURE);
  }
}

int main(int argc, char *argv[]) {
  FILE *f;
  int fsize, notify_len;
  const char *filename;
  char *code;
  char notify_msg[32];

  verify();
  struct printer p = {.sum1=0, .sum2=0, .quiet=getenv("QUIET") != NULL};

  if (argc < 2) {
    fprintf(stderr, "Expected filename\n");
    return EXIT_FAILURE;
  }

  filename = argv[1];

  f = fopen(filename, "r");
  if (f == NULL) {
    perror("bfc: fopen");
    return EXIT_FAILURE;
  }

  fseek(f, 0, SEEK_END);
  fsize = ftell(f);
  fseek(f, 0, SEEK_SET);

  code = malloc(sizeof(char) * fsize + 1);
  fread(code, 1, fsize, f);
  fclose(f);
  code[fsize] = 0;

  notify_len = snprintf(notify_msg, sizeof(notify_msg),
                        "C/" COMPILER "\t%d", getpid());
  notify(notify_msg, notify_len);
  run(code, &p);
  notify("stop", 4);

  free(code);

  if (p.quiet) {
    printf("Output checksum: %d\n", get_checksum(&p));
  }
}
