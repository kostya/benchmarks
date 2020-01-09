function StringIterator(str){
  this.str = str;
  this.current = 0;
  this.last = str.length - 1;

  this.done = function(){
    if (this.current > this.last)
      return true;
    else
      return false;
  };

  this.next = function(){
    if (this.current > this.last)
      throw StopIteration;
    else
      return this.str[this.current++];
  };
}

var Tape = function() {
  var pos = 0, tape = [0];
  this.inc = function(x) { tape[pos] += x; }
  this.move = function(x) { pos += x; while (pos >= tape.length) tape.push(0); }
  this.get = function() { return tape[pos]; }
}

const INC = 1;
const MOVE = 2;
const LOOP = 3;
const PRINT = 4;

function Op(op, v) {
  this.op = op;
  this.v = v;
}

var Brainfuck = function(text) {
  var me = this;

  var parse = function(iterator) {
    var res = [];
    while (!iterator.done()) {
      switch(iterator.next()) {
        case '+': res.push(new Op(INC, 1)); break;
        case '-': res.push(new Op(INC, -1)); break;
        case '>': res.push(new Op(MOVE, 1)); break;
        case '<': res.push(new Op(MOVE, -1)); break;
        case '.': res.push(new Op(PRINT, 0)); break;
        case '[': res.push(new Op(LOOP, parse(iterator))); break;
        case ']': return res;
      }
    }
    return res;
  }

  me.ops = parse(new StringIterator(text));

  var _run = function(ops, tape) {
    for (var i = 0; i < ops.length; i++) {
      var op = ops[i];
      switch(op.op) {
        case INC: tape.inc(op.v); break;
        case MOVE: tape.move(op.v); break;
        case LOOP: while (tape.get() > 0) _run(op.v, tape); break;
        case PRINT: process.stdout.write(String.fromCharCode(tape.get())); break;
      }
    }
  };

  me.run = function() {
    _run(me.ops, new Tape());
  };
}

function main(text) {
    var brainfuck = new Brainfuck(text);
    brainfuck.run();
}

function notify(msg) {
    return new Promise(resolve => {
        const client = require('net').connect(9001, 'localhost', () => {
            client.end(msg, 'utf8', () => {
                client.destroy();
                resolve();
            });
        }).on('error', resolve);
    });
}

(async function() {
    var text = require('fs').readFileSync(process.argv[2].toString()).toString();

    await notify(`Node.js\t${require('process').pid}`);
    main(text);
    await notify('stop');
})();
