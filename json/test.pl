use strict;
use warnings;
use File::Slurper 'read_binary';
use JSON::Tiny 'decode_json';

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
