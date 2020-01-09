<?php

function notify($msg) {
    $socket = @fsockopen('localhost', 9001);
    if ($socket) {
        fputs($socket, $msg);
        fclose($socket);
    }
}

$text = file_get_contents("/tmp/1.json");

$pid = posix_getpid();
notify("PHP\t$pid");

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

printf("%.8f\n", $x / $len);
printf("%.8f\n", $y / $len);
printf("%.8f\n", $z / $len);

notify("stop");
