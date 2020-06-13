const assert = require("assert");

function calc(text) {
    var jobj = JSON.parse(text);

    var coordinates = jobj['coordinates'];
    var len = coordinates.length;
    var x = 0;
    var y = 0;
    var z = 0;

    for (var i = 0; i < coordinates.length; i++) {
        coord = coordinates[i];
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
        const client = require('net').connect(9001, 'localhost', () => {
            client.end(msg, 'utf8', () => {
                client.destroy();
                resolve();
            });
        }).on('error', resolve);
    });
}

(async function() {
    left = calc(String.raw`{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}`);
    right = {x: 1.1, y: 2.2, z: 3.3};
    assert.deepStrictEqual(left, right);

    const text = require('fs').readFileSync("/tmp/1.json", "utf8");
    await notify(`Node.js\t${require('process').pid}`);
    console.log(calc(text));
    await notify('stop');
})().catch(err => console.error(err));
