'use strict';

const net = require('net');

const matrix = {
    new: function (n) {
        const a = new Array(n);
        for (let i = 0; i < n; i++) a[i] = new Float64Array(n);
        return a;
    },
    T: function (a, n) {
        const y = matrix.new(n);
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                y[i][j] = a[j][i];
            }
        }
        return y;
    },
    mul: function (a, b, n) {
        const y = matrix.new(n);
        const c = matrix.T(b, n);
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                let sum = 0;
                for (let k = 0; k < n; k++) sum = sum + a[i][k] * c[j][k];
                y[i][j] = sum;
            }
        }
        return y;
    }
};

function matgen(n, seed) {
    const y = matrix.new(n);
    const tmp = seed / n / n;
    for (let i = 0; i < n; i++) {
        for (let j = 0; j < n; j++) {
            y[i][j] = tmp * (i - j) * (i + j);
        }
    }
    return y;
}

function calc(n) {
    n = n >> 1 << 1;
    const a = matgen(n, 1.0);
    const b = matgen(n, 2.0);
    const c = matrix.mul(a, b, n);
    return c[n / 2][n / 2];
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
    const n = process.argv.length > 1 ? parseInt(process.argv[2]) : 100;

    const left = calc(101);
    const right = -18.67;
    if (Math.abs(left - right) > 0.1) {
        console.error(`${left} != ${right}`)
        process.exit(1);
    }

    await notify(`Node.js\t${process.pid}`);
    const results = calc(n);
    await notify('stop');

    console.log(results);
})();
