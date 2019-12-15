<?php
$STR_SIZE = 131072;
$TRIES = 8192;

$str = str_repeat("a", $STR_SIZE);

$socket = @fsockopen('localhost', 9001);
if ($socket) {
   fputs($socket, 'PHP');
   fclose($socket);
}

$str2 = base64_encode($str);
printf("encode %s... to %s...: ", substr($str, 0, 4), substr($str2, 0, 4));

$s = 0;
$start = microtime(true);
for ($i = 0; $i < $TRIES; $i++) {
    $str2 = base64_encode($str);
    $s += strlen($str2);
}
printf("%d, %.2f\n", $s, microtime(true) - $start);

$str3 = base64_decode($str2);
printf("decode %s... to %s...: ", substr($str2, 0, 4), substr($str3, 0, 4));

$s = 0;
$start = microtime(true);
for ($i = 0; $i < $TRIES; $i++) {
    $str3 = base64_decode($str2);
    $s += strlen($str3);
}
printf("%d, %.2f\n", $s, microtime(true) - $start);
