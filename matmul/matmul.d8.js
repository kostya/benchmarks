var matrix = {}

matrix.new = function (n) {
    var a = new Array(n);
    for (var i = 0; i < n; i++) a[i] = new Float64Array(n);
    return a;
}

matrix.T = function (a, n) {
    var y = matrix.new(n)
    for (var i = 0; i < n; i++) {
        for (var j = 0; j < n; j++) {
            y[i][j] = a[j][i]
        }
    }
    return y
}

matrix.mul = function (a, b, n) {
    var y = matrix.new(n)
    var c = matrix.T(b, n)
    for (var i = 0; i < n; i++) {
        for (var j = 0; j < n; j++) {
            var sum = 0;
            for (var k = 0; k < n; k++) sum = sum + a[i][k] * c[j][k]
            y[i][j] = sum;
        }
    }
    return y
}

matgen = function (n) {
    var y = matrix.new(n)
    var tmp = 1 / n / n
    for (var i = 0; i < n; i++) {
        for (var j = 0; j < n; j++) {
            y[i][j] = tmp * (i - j) * (i + j)
        }
    }
    return y
}

var n = 100;
if (arguments[0]) n = parseInt(arguments[0]);

var a = matgen(n);
var b = matgen(n);
var c = matrix.mul(a, b, n);
print(c[(n/2)][(n / 2)]);
