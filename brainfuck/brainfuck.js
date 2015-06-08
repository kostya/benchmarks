var Tape = function() {
	var pos = 0, tape = [0];

	this.inc = function() { tape[pos]++; }
	this.dec = function() { tape[pos]--; }
	this.advance = function() { pos++; if (tape.length <= pos) tape.push(0); }
	this.devance = function() { if (pos > 0) pos--; }
	this.get = function() { return tape[pos]; }
}

var Brainfuck = function(text) {
	var me = this;
	me.code = "";
	me.maps = [];

	var leftstack = [];
	for (var i = 0, pc = 0; i < text.length; i++) if ("+-<>[].,".indexOf(text.charAt(i)) != -1) { me.code += text.charAt(i); me.maps.push(0); }
	for (var pc = 0; pc < me.code.length; pc++) {
		c = me.code[pc][0];
		if (c === '[') leftstack.push(pc);
		if (c === ']' && leftstack.length > 0) {
			var left = leftstack.pop();
			me.maps[left] = pc;
			me.maps[pc] = left;
		}
	}

	me.run = function() {
		var tape = new Tape();
		var code = this.code;
		var maps = this.maps;
		var length = this.code.length;

		for (var pc = 0; pc < length; pc++)
			switch(code[pc]) {
				case '+': tape.inc(); break;
				case '-': tape.dec(); break;
				case '>': tape.advance(); break;
				case '<': tape.devance(); break;
				case '[': if (tape.get() == 0) pc = maps[pc]; break;
				case ']': if (tape.get() != 0) pc = maps[pc]; break;
				case '.': process.stdout.write(String.fromCharCode(tape.get()));
				default:
			}
	};
}

var text = require('fs').readFileSync(process.argv[2]).toString();
var brainfuck = new Brainfuck(text);
brainfuck.run();
