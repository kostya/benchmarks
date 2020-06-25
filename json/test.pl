use v5.12;
use warnings;
use feature qw(signatures);
no warnings qw(experimental::signatures);

use File::Slurper 'read_binary';
use JSON::Tiny 'decode_json';
use Socket;
use Class::Struct;
use Data::Dumper;
use Test::More tests => 1;

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

    my $left = calc('{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}');
    my $right = Coordinate->new(x=>1.1, y=>2.2, z=>3.3);
    my $ok = is_deeply($left, $right);
    if (!$ok) {
        print STDERR "@{[ Dumper($left) ]} != @{[ Dumper($right) ]}\n";
        exit(1);
    }

    my $bytes = read_binary '/tmp/1.json';

    my $pid = $$;
    notify("Perl JSON::Tiny\t${pid}");

    print Dumper(calc($bytes)), "\n";

    notify("stop");
}
