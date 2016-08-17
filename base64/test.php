<?php
$STR_SIZE = 10000000;
$TRIES = 100;

$str = "";
for ($i = 0; $i < $STR_SIZE; $i++) $str .= "a";
$str2 = "";

$s = 0;
$start = microtime(true);
for ($i = 0; $i < $TRIES; $i++) {
    $str2 = base64_encode($str);
    $s += strlen($str2);
}
printf("encode: %d, %.2f\n", $s, microtime(true) - $start);


$s = 0;
$start = microtime(true);
for ($i = 0; $i < $TRIES; $i++) {
    $str3 = base64_decode($str2);
    $s += strlen($str3);
}
printf("decode: %d, %.2f\n", $s, microtime(true) - $start);
