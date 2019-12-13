use strict;
use warnings;
use File::Slurper 'read_binary';
use Cpanel::JSON::XS 'decode_json';
use Socket;

socket(my $socket, PF_INET, SOCK_STREAM,(getprotobyname('tcp'))[2]);
if (connect($socket, pack_sockaddr_in(9001, inet_aton('localhost')))) {
  print $socket "Perl Cpanel::JSON::XS";
}
close($socket);

my $jobj = decode_json read_binary '1.json';
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
