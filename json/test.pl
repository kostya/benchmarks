use strict;
use warnings;
use File::Slurper 'read_binary';
use JSON::Tiny 'decode_json';
use Socket;

sub notify {
    my $msg = shift;
    socket(my $socket, Socket::PF_INET, Socket::SOCK_STREAM, (getprotobyname('tcp'))[2]);
    if (connect($socket, Socket::pack_sockaddr_in(9001, Socket::inet_aton('localhost')))) {
        print $socket $msg;
    }
    close($socket);
}

my $bytes = read_binary '/tmp/1.json';

my $pid = $$;
notify("Perl JSON::Tiny\t${pid}");

my $jobj = decode_json $bytes;
my $coordinates = $jobj->{coordinates};
my $len = @$coordinates;
my $x = my $y = my $z = 0;

foreach my $coord (@$coordinates) {
  $x += $coord->{x};
  $y += $coord->{y};
  $z += $coord->{z};
}

print $x / $len, "\n";
print $y / $len, "\n";
print $z / $len, "\n";

notify("stop");
