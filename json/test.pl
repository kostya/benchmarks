use v5.32;
use warnings;
no feature qw(indirect);
use feature qw(signatures);
no warnings qw(experimental);

use File::Slurper 'read_binary';
use JSON::Tiny 'decode_json';
use Socket;
use Class::Struct;
use Data::Dumper;
use Test::More tests => 2;

struct(Coordinate => [x => '$', y => '$', z => '$']);

sub notify ($msg) {
    socket(my $socket, Socket::PF_INET, Socket::SOCK_STREAM, (getprotobyname('tcp'))[2]);
    if (connect($socket, Socket::pack_sockaddr_in(9001, Socket::inet_aton('localhost')))) {
        print $socket $msg;
    }
    close($socket);
}

sub calc ($bytes) {
    my $jobj = decode_json $bytes;
    my $coordinates = $jobj->{coordinates};
    my $len = @$coordinates;
    my $x = my $y = my $z = 0;

    foreach my $coord (@$coordinates) {
        $x += $coord->{x};
        $y += $coord->{y};
        $z += $coord->{z};
    }

    return Coordinate->new(x=>$x / $len, y=>$y / $len, z=>$z / $len);
}

if ($0 eq __FILE__) {
    $Data::Dumper::Terse = 1;
    $Data::Dumper::Indent = 0;

    my $right = Coordinate->new(x=>2.0, y=>0.5, z=>0.25);
    foreach('{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}',
            '{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}') {
        my $left = calc($_);
        my $ok = is_deeply($left, $right);
        if (!$ok) {
            print STDERR "@{[ Dumper($left) ]} != @{[ Dumper($right) ]}\n";
            exit(1);
        }
    }

    my $bytes = read_binary '/tmp/1.json';

    notify("Perl (JSON::Tiny)\t" . $$);
    my $results = calc($bytes);
    notify("stop");

    print Dumper($results), "\n";
}
