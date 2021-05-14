'use strict';

const net = require('net');

const UPPER_BOUND = 5000000;
const PREFIX = 32338;

class Node {
    constructor() {
        this.children = new Map();
        this.terminal = false;
    }
}

const toInt = function(n) {
    return n >>> 0;
}

function generatePrimes(limit) {
    const prime = new Int8Array(limit + 1);

    for (let x = 1; x * x < limit; ++x) {
        for (let y = 1; y * y < limit; ++y) {
            let n = (4 * x * x) + (y * y);
            if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
                prime[n] = !prime[n];
            }

            n = (3 * x * x) + (y * y);
            if (n <= limit && n % 12 == 7) {
                prime[n] = !prime[n];
            }

            n = (3 * x * x) - (y * y);
            if (x > y && n <= limit && n % 12 == 11) {
                prime[n] = !prime[n];
            }
        }
    }

    for (let r = 5; r * r < limit; ++r) {
        if (prime[r]) {
            for (let i = r * r; i < limit; i += r * r) {
                prime[i] = 0;
            }
        }
    }

    const result = [2, 3];
    for (let p = 5; p <= limit; ++p) {
        if (prime[p]) {
            result.push(p);
        }
    }
    return result;
}

function generateTrie(l) {
    const root = new Node()
    for (const el of l) {
        let head = root;
        for (const ch of el.toString()) {
            if (!head.children.has(ch)) {
                head.children.set(ch, new Node());
            }
            head = head.children.get(ch);
        }
        head.terminal = true;
    }
    return root;
}

function find(upperBound, prefix) {
    const primes = generatePrimes(upperBound);
    const root = generateTrie(primes);
    const strPrefix = prefix.toString();
    let head = root;

    for (const ch of strPrefix) {
        head = head.children.get(ch);
        if (typeof head === 'undefined') {
            return null;
        }
    }

    let [queue, result] = [[[head, strPrefix]], []]
    while (queue.length > 0) {
        const [top, prefix] = queue.pop();
        if (top.terminal) {
            result.push(toInt(prefix));
        }
        for (const [ch, v] of top.children) {
            queue.splice(0, 0, [v, prefix + ch]);
        }
    }
    return result;
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

function verify() {
    const left = [2, 23, 29];
    const right = find(100, 2);
    if (left.toString() != right.toString()) {
        console.error(`${left} != ${right}`)
        process.exit(1);
    }
}

(async function() {
    verify();
    await notify(`Node.js\t${process.pid}`);
    const results = find(UPPER_BOUND, PREFIX);
    await notify('stop');

    console.log(results);
})();
