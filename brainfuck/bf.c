#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libnotify.h>

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

void op_list_init(struct op_list *list)
{
	list->len = 0;
	list->cap = 0;
	list->ops = NULL;
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

void string_iterator_init(struct string_iterator *it, const char *s)
{
	it->string = s;
	it->pos = 0;
}

char string_iterator_next(struct string_iterator *it)
{
	return it->string[it->pos++];
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
			loop_ops = malloc(sizeof(struct op_list));
			op_list_init(loop_ops);
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

void tape_init(struct tape *tape)
{
	tape->pos = 0;
	tape->cap = 1;
	tape->tape = malloc(sizeof(int));
	*tape->tape = 0;
}

void tape_free(struct tape tape)
{
	free(tape.tape);
}

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

void eval(const struct op_list *ops, struct tape *tape)
{
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
				eval(op.value.list, tape);
			break;
		case OP_PRINT:
			putc(tape_get(*tape), stdout);
			fflush(stdout);
			break;
		}
	}
}

int main(int argc, char *argv[]) {
	FILE *f;
	int fsize, notify_len;
	const char *filename;
	char *code;
	char notify_msg[32];
	struct op_list ops;
	struct tape tape;
	struct string_iterator it;

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

	tape_init(&tape);
	op_list_init(&ops);
	string_iterator_init(&it, code);
	parse(&it, &ops);

	eval(&ops, &tape);

	free(code);
	tape_free(tape);
	op_list_free(&ops);

	notify("stop", 4);
}
