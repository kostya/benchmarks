use strict;
use warnings;
use MIME::Base64 qw(encode_base64 decode_base64);
use Time::HiRes 'time';

use constant STR_SIZE => 10_000_000;
use constant TRIES => 100;

my $str = 'a' x STR_SIZE;
my $str2 = '';

my ($t, $s) = (time, 0);
for (0..TRIES) {
  $str2 = encode_base64 $str, '';
  $s += length $str2;
}
print "encode: $s, ", (time - $t), "\n";

($t, $s) = (time, 0);
for (0..TRIES) {
  $s += length decode_base64 $str2;
}
print "decode: $s, ", (time - $t), "\n";
