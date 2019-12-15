use strict;
use warnings;
use MIME::Base64::Perl qw(encode_base64 decode_base64);
use Time::HiRes 'time';
use Socket;

use constant STR_SIZE => 131072;
use constant TRIES => 8192;

my $str = 'a' x STR_SIZE;

socket(my $socket, PF_INET, SOCK_STREAM,(getprotobyname('tcp'))[2]);
if (connect($socket, pack_sockaddr_in(9001, inet_aton('localhost')))) {
  print $socket "Perl MIME::Base64::Perl";
}
close($socket);

my $str2 = encode_base64 $str, '';
printf("encode %s... to %s...: ",
       substr($str, 0, 4), substr($str2, 0, 4));

my ($t, $s) = (time, 0);
for (1..TRIES) {
  $str2 = encode_base64 $str, '';
  $s += length $str2;
}
printf("%d, %.2f\n", $s, time - $t);

my $str3 = decode_base64 $str2;
printf("decode %s... to %s...: ",
       substr($str2, 0, 4), substr($str3, 0, 4));

($t, $s) = (time, 0);
for (1..TRIES) {
  $str3 = decode_base64 $str2;
  $s += length $str3;
}
printf("%d, %.2f\n", $s, time - $t);
