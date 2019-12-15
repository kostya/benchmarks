const util = require('util');

const STR_SIZE = 131072;
const TRIES = 8192;

const b = Buffer.from("a".repeat(STR_SIZE));

function main() {
    var str2 = b.toString('base64');
    process.stdout.write(
        util.format("encode %s... to %s...: ",
		    b.toString('utf8', 0, 4),
		    str2.substring(0, 4)));

    var s = 0;
    var start = new Date();
    for (var i = 0; i < TRIES; i++) {
        str2 = b.toString('base64');
        s += str2.length;
    }
    console.log("%d, %d", s, ((new Date()) - start) / 1000);

    var str3 = Buffer.from(str2, 'base64');
    process.stdout.write(
        util.format("decode %s... to %s...: ",
		    str2.substring(0, 4),
		    str3.toString('utf8', 0, 4)));

    start = new Date();
    s = 0;
    for (var i = 0; i < TRIES; i++) {
        str3 = Buffer.from(str2, 'base64');
        s += str3.length;
    }
    console.log("%d, %d", s, ((new Date()) - start) / 1000);
}

const client = require('net').connect(9001, 'localhost', () => {
    client.end('Node.js', 'utf8', () => {
        client.destroy();
        main();
    });
}).on('error', main);
