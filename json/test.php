<?php

class Coordinate {
    public $x, $y, $z;

    function __construct(float $x, float $y, float $z) {
        $this->x = $x;
        $this->y = $y;
        $this->z = $z;
    }
}

function notify($msg) {
    $socket = @fsockopen('localhost', 9001);
    if ($socket) {
        fputs($socket, $msg);
        fclose($socket);
    }
}

function calc($text) {
    $jobj = json_decode($text, true);

    $coordinates = $jobj['coordinates'];
    $len = count($coordinates);
    $x = 0;
    $y = 0;
    $z = 0;

    for ($i = 0; $i < $len; $i++) {
        $coord = $coordinates[$i];
        $x += $coord['x'];
        $y += $coord['y'];
        $z += $coord['z'];
    }

    return new Coordinate($x / $len, $y / $len, $z / $len);
}

if (isset($argv[0]) && realpath($argv[0]) == __FILE__) {
    $right = new Coordinate(1.1, 2.2, 3.3);
    foreach(['{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}',
             '{"coordinates":[{"y":2.2,"x":1.1,"z":3.3}]}'] as &$v) {
        $left = calc($v);
        if ($left != $right) {
            exit(
                sprintf("%s != %s",
                        print_r($left, TRUE),
                        print_r($right, TRUE)));
        }
    }

    $text = file_get_contents("/tmp/1.json");

    $pid = posix_getpid();
    notify("PHP\t$pid");

    print_r(calc($text));

    notify("stop");
}
