const STR_SIZE = 10000000;
const TRIES = 100;

const str = "a".repeat(STR_SIZE);
var str2 = "";
const b = Buffer.from(str);

var s = 0;
var start = new Date();
for (var i = 0; i < TRIES; i++) {
  str2 = b.toString('base64');
  s += str2.length;
}
console.log("encode: %d, %d", s, ((new Date()) - start) / 1000);

start = new Date();
s = 0
for (var i = 0; i < TRIES; i++) {
  const b = Buffer.from(str2, 'base64');
  s += b.length;
}
console.log("decode: %d, %d", s, ((new Date()) - start) / 1000);
