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
	me.bracket_map = function(text) {
		var leftstack = [];
		var bm = {};

		for (var i = 0, pc = 0; i < text.length; i++) {
			var c = text.charAt(i);
			if ("+-<>[].,".indexOf(c) === -1) continue;
			if (c === '[') leftstack.push(pc);
			if (c === ']' && leftstack.length > 0) {
				var left = leftstack.pop();
				bm[left] = pc;
				bm[pc] = left;
 			}
 			me.code += c;
 			pc++;
		}
		return bm;
	}(text);

	me.run = function() {
		var pc = 0;
		var tape = new Tape();
		var code = this.code;
		var bm = this.bracket_map;

		for (var pc = 0; pc < code.length; pc++)
			switch(code[pc]) {
				case '+': tape.inc(); break;
				case '-': tape.dec(); break;
				case '>': tape.advance(); break;
				case '<': tape.devance(); break;
				case '[': if (tape.get() == 0) pc = bm[pc]; break;
				case ']': if (tape.get() != 0) pc = bm[pc]; break;
				case '.': process.stdout.write(String.fromCharCode(tape.get()));
				default:
			}
	};

}

var text = require('fs').readFileSync(process.argv[2].toString()).toString(); 
var brainfuck = new Brainfuck(text);
brainfuck.run();
