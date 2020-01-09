function main(text) {
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

    console.log(x / len);
    console.log(y / len);
    console.log(z / len);
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
    const text = require('fs').readFileSync("/tmp/1.json", "utf8");
    await notify(`Node.js\t${require('process').pid}`);
    main(text);
    await notify('stop');
})();
