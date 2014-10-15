var STR_SIZE = 10000000;
var TRIES = 100;

var str = ""; for (var i = 0; i < STR_SIZE; i++) str += "a";
var str2 = "";

var s = 0;
var start = new Date();
for (var i = 0; i < TRIES; i++) {
  var b = new Buffer(str);
  str2 = b.toString('base64');
  s += str2.length;
}
console.log("encode: %d, %d", s, ((new Date()) - start) / 1000);

start = new Date();
s = 0
for (var i = 0; i < TRIES; i++) {
  var b = new Buffer(str2, 'base64');
  var str3 = b.toString();
  s += str3.length;
}
console.log("decode: %d, %d", s, ((new Date()) - start) / 1000);
