# rurban; distributed under the MIT license

sub matmul {
  my ($a, $b) = @_;
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

sub matgen {
  my $n = shift;
  my $tmp = 1.0 / $n / $n;
  my @a;
  for my $i (0..$n) {
    for my $j (0..$n) {
      $a[$i]->[$j] = $tmp * ($i - $j) * ($i + $j);
    }
  }
  \@a
}

my $n = @ARGV ? shift : 100;
$n = $n / 2 * 2;
my $a = matgen($n);
my $b = matgen($n);
my $c = matmul($a, $b);
print $c->[$n/2]->[$n/2], "\n";
