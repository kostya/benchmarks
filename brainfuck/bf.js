'use strict';

const assert = require("assert");
const fs = require("fs");
const net = require('net');

class StringIterator {
    constructor(str) {
        this.str = str;
        this.current = 0;
        this.last = str.length - 1;
    }

    done() {
        return this.current > this.last;
    }

    next() {
        if (this.current > this.last)
            throw StopIteration;
        else
            return this.str[this.current++];
    }
}

class Tape {
    constructor() {
        this.pos = 0;
        this.tape = [0];
    }

    inc(x) {
        this.tape[this.pos] += x;
    }

    move(x) {
        this.pos += x;
        while (this.pos >= this.tape.length) this.tape.push(0);
    }

    get() {
        return this.tape[this.pos];
    }
}

class Printer {
    constructor(quiet) {
        this.sum1 = 0;
        this.sum2 = 0;
        this.quiet = quiet;
    }

    print(n) {
        if (this.quiet) {
            this.sum1 = (this.sum1 + n) % 255;
            this.sum2 = (this.sum2 + this.sum1) % 255;
        } else {
            process.stdout.write(String.fromCharCode(n));
        }
    }

    get checksum() {
        return (this.sum2 << 8) | this.sum1;
    }
}

const INC = 1;
const MOVE = 2;
const LOOP = 3;
const PRINT = 4;

class Op {
    constructor(op, v) {
        this.op = op;
        this.v = v;
    }
}

class Brainfuck {
    constructor(text, p) {
        this.ops = this.parse(new StringIterator(text));
        this.p = p;
    }

    parse(iterator) {
        var res = [];
        while (!iterator.done()) {
            switch(iterator.next()) {
            case '+': res.push(new Op(INC, 1)); break;
            case '-': res.push(new Op(INC, -1)); break;
            case '>': res.push(new Op(MOVE, 1)); break;
            case '<': res.push(new Op(MOVE, -1)); break;
            case '.': res.push(new Op(PRINT, 0)); break;
            case '[': res.push(new Op(LOOP, this.parse(iterator))); break;
            case ']': return res;
            }
        }
        return res;
    }

    _run(ops, tape) {
        for (var i = 0; i < ops.length; i++) {
            const op = ops[i];
            switch(op.op) {
            case INC: tape.inc(op.v); break;
            case MOVE: tape.move(op.v); break;
            case LOOP: while (tape.get() > 0) this._run(op.v, tape); break;
            case PRINT: this.p.print(tape.get()); break;
            }
        }
    }

    run() {
        this._run(this.ops, new Tape());
    }
}

function notify(msg) {
    return new Promise(resolve => {
        const client = net.connect(9001, 'localhost', () => {
            client.end(msg, 'utf8', () => {
                client.destroy();
                resolve();
            });
        }).on('error', resolve);
    });
}

function verify() {
    const text = `++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.`;
    const p_left= new Printer(true);
    new Brainfuck(text, p_left).run();
    const left = p_left.checksum;

    const p_right = new Printer(true);
    for (const c of "Hello World!\n") {
        p_right.print(c.charCodeAt(0));
    }
    const right = p_right.checksum;
    assert.deepStrictEqual(left, right);
}

(async function() {
    verify();
    const text = fs.readFileSync(process.argv[2].toString()).toString();
    const p = new Printer(process.env.QUIET);

    await notify(`Node.js\t${process.pid}`);
    new Brainfuck(text, p).run();
    await notify('stop');

    if (p.quiet) {
        console.log(`Output checksum: ${p.checksum}`)
    }
})();
