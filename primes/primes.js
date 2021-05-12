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

function generatePrimes(m) {
    let result = new Set([2]);
    for (let i = 1; i < toInt(1 + (m - 1) / 2); i++) {
        result.add(2 * i + 1);
    }
    let [k, j] = [1, 1];
    const sqr = function(i) { return i * i; }
    const maxN = function(i) {
        return toInt((m - sqr(2 * i + 1)) / (4 * i + 2));
    }
    while (k > 0) {
        k = maxN(j++);
    }
    k = j;
    for (let i = 1; i < k + 1; i++) {
        for (let n = 0; n <maxN(i - 1); n++) {
            result.delete((2 * i + 1) * (2 * i + 2 * n + 1));
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
