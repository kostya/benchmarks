# rurban; distributed under the MIT license
use v5.32;
use warnings;
no feature qw(indirect);
use feature qw(signatures);
no warnings qw(experimental);

use Socket;

sub matmul ($a, $b) {
    my $m = $#{$a} - 1;
    my $n = $#{$a->[0]} - 1;
    my $p = $#{$b->[0]} - 1;

    # transpose
    my @b2;
    for my $i (0..$n) {
        for my $j (0..$p) {
            $b2[$j]->[$i] = $b->[$i]->[$j];
        }
    }

    # multiplication
    my @c;
    for my $i (0..$m) {
        for my $j (0..$p) {
            my $s = 0;
            my ($ai, $b2j) = ($a->[$i], $b2[$j]);
            for my $k (0..$n) {
                $s += $ai->[$k] * $b2j->[$k];
            }
            $c[$i]->[$j] = $s;
        }
    }
    \@c
}

sub matgen ($n, $seed) {
    my $tmp = $seed / $n / $n;
    my @a;
    for my $i (0..$n) {
        for my $j (0..$n) {
            $a[$i]->[$j] = $tmp * ($i - $j) * ($i + $j);
        }
    }
    \@a
}

sub notify ($msg) {
    socket(my $socket, Socket::PF_INET, Socket::SOCK_STREAM, (getprotobyname('tcp'))[2]);
    if (connect($socket, Socket::pack_sockaddr_in(9001, Socket::inet_aton('localhost')))) {
        print $socket $msg;
    }
    close($socket);
}

sub calc ($n) {
    $n = int($n / 2) * 2;
    my $a = matgen($n, 1.0);
    my $b = matgen($n, 2.0);
    my $c = matmul($a, $b);
    $c->[$n/2]->[$n/2]
}

if ($0 eq __FILE__) {
    my $n = @ARGV ? shift : 100;

    my $left = calc(101);
    my $right = -18.67;
    if (abs($left - $right) > 0.1) {
        print STDERR "${left} != ${right}\n";
        exit(1);
    }

    notify("Perl\t" . $$);
    my $results = calc($n);
    notify("stop");

    print $results, "\n";
}
