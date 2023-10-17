<?php

class StringIterator {
    function __construct($str) {
        $this->str = $str;
        $this->current = 0;
        $this->last = strlen($str) - 1;
    }

    function done() {
        return $this->current > $this->last;
    }

    function next() {
        if ($this->current > $this->last)
            throw new Exception("StopIteration");
        else
            return $this->str[$this->current++];
    }
}

class Tape {
    function __construct() {
        $this->pos = 0;
        $this->tape = [0];
    }

    function inc($x) {
        $this->tape[$this->pos] += $x;
    }

    function move($x) {
        $this->pos += $x;
        while ($this->pos >= count($this->tape)) $this->tape []= 0;
    }

    function get() {
        return $this->tape[$this->pos];
    }
}

class Printer {
    function __construct($quiet) {
        $this->sum1 = 0;
        $this->sum2 = 0;
        $this->quiet = $quiet;
    }

    function print($n) {
        if ($this->quiet) {
            $this->sum1 = ($this->sum1 + $n) % 255;
            $this->sum2 = ($this->sum2 + $this->sum1) % 255;
        } else {
            echo chr($n);
        }
    }

    function checksum() {
        return ($this->sum2 << 8) | $this->sum1;
    }
}


class Op {
    function __construct($op, $v) {
        $this->op = $op;
        $this->v = $v;
    }
}

class Brainfuck {
    const INC = 1;
    const MOVE = 2;
    const LOOP = 3;
    const PRINT = 4;

    function __construct($text, $p) {
        $this->ops = $this->parse(new StringIterator($text));
        $this->p = $p;
    }

    function parse($iterator) {
        $res = [];
        while (!$iterator->done()) {
            switch($iterator->next()) {
            case '+': $res []= new Op(self::INC, 1); break;
            case '-': $res []= new Op(self::INC, -1); break;
            case '>': $res []= new Op(self::MOVE, 1); break;
            case '<': $res []= new Op(self::MOVE, -1); break;
            case '.': $res []= new Op(self::PRINT, 0); break;
            case '[': $res []= new Op(self::LOOP, $this->parse($iterator)); break;
            case ']': return $res;
            }
        }
        return $res;
    }

    function _run($ops, $tape) {
        foreach($ops as $op) {
            switch($op->op) {
            case self::INC: $tape->inc($op->v); break;
            case self::MOVE: $tape->move($op->v); break;
            case self::LOOP: while ($tape->get() > 0) $this->_run($op->v, $tape); break;
            case self::PRINT: $this->p->print($tape->get()); break;
            }
        }
    }

    function run() {
        $this->_run($this->ops, new Tape());
    }
}

function notify($msg) {
    $fp = stream_socket_client("tcp://localhost:9001", $errno, $errstr);
    if (!$fp) die("$errstr ($errno)");
    fwrite($fp, $msg);
    fclose($fp);
}

function verify() {
    $text = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    $p_left = new Printer(true);
    (new Brainfuck($text, $p_left))->run();
    $left = $p_left->checksum();

    $p_right = new Printer(true);
    foreach(str_split("Hello World!\n") as $c) {
        $p_right->print(ord($c));
    }
    $right = $p_right->checksum();
    assert($left == $right);
}

verify();
$text = file_get_contents($argv[1]);
$p = new Printer(@$_SERVER["QUIET"]);

notify("PHP\t" . getmypid());
(new Brainfuck($text, $p))->run();
notify('stop');

if ($p->quiet) {
    echo "Output checksum: ".$p->checksum()."\n";
}
