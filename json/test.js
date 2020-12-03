'use strict';

const assert = require("assert");
const fs = require("fs");
const net = require('net');

function calc(text) {
    const jobj = JSON.parse(text);

    const coordinates = jobj['coordinates'];
    const len = coordinates.length;
    let x = 0;
    let y = 0;
    let z = 0;

    for (let i = 0; i < coordinates.length; i++) {
        const coord = coordinates[i];
        x += coord['x'];
        y += coord['y'];
        z += coord['z'];
    }

    return {
        x: x / len,
        y: y / len,
        z: z / len
    };
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
    const right = {x: 2.0, y: 0.5, z: 0.25};
    [String.raw`{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
     String.raw`{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`].forEach(
         function(v) {
             const left = calc(v);
             assert.deepStrictEqual(left, right);
         });

    const text = fs.readFileSync("/tmp/1.json", "utf8");

    await notify(`Node.js\t${process.pid}`);
    const results = calc(text);
    await notify('stop');

    console.log(results);
})().catch(err => console.error(err));
