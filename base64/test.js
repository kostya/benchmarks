'use strict';

const assert = require("assert");
const net = require('net');
const util = require('util');

const STR_SIZE = 131072;
const TRIES = 8192;

async function main() {
    const b = Buffer.from("a".repeat(STR_SIZE));
    const str2 = b.toString('base64');
    const str3 = Buffer.from(str2, 'base64');

    await notify(`Node.js\t${process.pid}`);

    var s_encoded = 0;
    const start = new Date();
    for (var i = 0; i < TRIES; i++) {
        s_encoded += b.toString('base64').length;
    }
    const t_encoded = ((new Date()) - start) / 1000;

    var s_decoded = 0;
    const start1 = new Date();
    for (var i = 0; i < TRIES; i++) {
        s_decoded += Buffer.from(str2, 'base64').length;
    }
    const t_decoded = ((new Date()) - start1) / 1000;

    await notify('stop');

    console.log(util.format("encode %s... to %s...: %d, %d",
                            b.toString('utf8', 0, 4),
                            str2.substring(0, 4),
                            s_encoded, t_encoded));

    console.log(util.format("decode %s... to %s...: %d, %d",
                            str2.substring(0, 4),
                            str3.toString('utf8', 0, 4),
                            s_decoded, t_decoded));
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

(async function() {
    for (const [src, dst] of [["hello", "aGVsbG8="], ["world", "d29ybGQ="]]) {
        const encoded = Buffer.from(src, 'utf8').toString('base64');
        assert.deepStrictEqual(encoded, dst);
        const decoded = Buffer.from(dst, 'base64').toString();
        assert.deepStrictEqual(decoded, src);
    }

    await main();
})();
