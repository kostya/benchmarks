use v5.32;
use warnings;
no feature qw(indirect);
use feature qw(signatures);
no warnings qw(experimental);

use MIME::Base64 qw(encode_base64 decode_base64);
use Time::HiRes 'time';
use Socket;
use Data::Dumper;
use Test::More tests => 4;

sub notify ($msg) {
    socket(my $socket, Socket::PF_INET, Socket::SOCK_STREAM, (getprotobyname('tcp'))[2]);
    if (connect($socket, Socket::pack_sockaddr_in(9001, Socket::inet_aton('localhost')))) {
        print $socket $msg;
    }
    close($socket);
}

if ($0 eq __FILE__) {
    for ((["hello", "aGVsbG8="], ["world", "d29ybGQ="])) {
        my ($src, $dst) = @$_;
        my $encoded = encode_base64 $src, '';
        if (!is_deeply($encoded, $dst)) {
            print STDERR "@{[ Dumper($encoded) ]} != @{[ Dumper($dst) ]}\n";
            exit(1);
        }
        my $decoded = decode_base64 $dst;
        if (!is_deeply($decoded, $src)) {
            print STDERR "@{[ Dumper($decoded) ]} != @{[ Dumper($src) ]}\n";
            exit(1);
        }
    }

    use constant STR_SIZE => 131072;
    use constant TRIES => 8192;

    my $str = 'a' x STR_SIZE;
    my $str2 = encode_base64 $str, '';
    my $str3 = decode_base64 $str2;

    notify("Perl (MIME::Base64)\t". $$);

    my ($t, $s_encoded) = (time, 0);
    for (1..TRIES) {
        $s_encoded += length encode_base64 $str, '';
    }
    my $t_encoded = time - $t;

    my ($t1, $s_decoded) = (time, 0);
    for (1..TRIES) {
        $s_decoded += length decode_base64 $str2;
    }
    my $t_decoded = time - $t1;

    notify("stop");

    printf("encode %s... to %s...: %d, %.2f\n",
           substr($str, 0, 4), substr($str2, 0, 4),
        $s_encoded, $t_encoded);
    printf("decode %s... to %s...: %d, %.2f\n",
           substr($str2, 0, 4), substr($str3, 0, 4),
           $s_decoded, $t_decoded);
}
