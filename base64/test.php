<?php

function notify($msg) {
    $socket = @fsockopen('localhost', 9001);
    if ($socket) {
        fputs($socket, $msg);
        fclose($socket);
    }
}

if (isset($argv[0]) && realpath($argv[0]) == __FILE__) {
    foreach([["hello", "aGVsbG8="], ["world", "d29ybGQ="]] as [$src, $dst]) {
        $encoded = base64_encode($src);
        if ($encoded != $dst) {
            exit(sprintf("%s != %s",
                         print_r($encoded, TRUE),
                         print_r($dst, TRUE)));
        }
        $decoded = base64_decode($dst);
        if ($decoded != $src) {
            exit(sprintf("%s != %s",
                         print_r($decoded, TRUE),
                         print_r($src, TRUE)));
        }
    }

    $STR_SIZE = 131072;
    $TRIES = 8192;

    $str = str_repeat("a", $STR_SIZE);
    $str2 = base64_encode($str);
    $str3 = base64_decode($str2);

    $pid = posix_getpid();
    notify("PHP\t$pid");

    $s_encoded = 0;
    $start = microtime(true);
    for ($i = 0; $i < $TRIES; $i++) {
        $s_encoded += strlen(base64_encode($str));
    }
    $t_encoded = microtime(true) - $start;

    $s_decoded = 0;
    $start = microtime(true);
    for ($i = 0; $i < $TRIES; $i++) {
        $s_decoded += strlen(base64_decode($str2));
    }
    $t_decoded = microtime(true) - $start;

    notify("stop");

    printf("encode %s... to %s...: %d, %.2f\n",
           substr($str, 0, 4), substr($str2, 0, 4),
           $s_encoded, $t_encoded);
    printf("decode %s... to %s...: %d, %.2f\n",
           substr($str2, 0, 4), substr($str3, 0, 4),
           $s_decoded, $t_decoded);
}
