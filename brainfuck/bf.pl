use v5.32;
use warnings;
no feature qw(indirect);
use feature qw(signatures);
no warnings qw(experimental);

use Socket;

package Op;

sub new ($class, $op, $val = undef) {
    my $self = {
        op => $op,
        val => $val,
    };
    bless $self, $class;
    return $self;
}


package Tape;

sub new ($class) {
    my $self = {
        tape => [0],
        pos => 0,
    };
    bless $self, $class;
    return $self;
}

sub get ($self) {
    return $self->{tape}[$self->{pos}];
}

sub inc ($self, $x) {
    $self->{tape}[$self->{pos}] += $x;
}

sub move ($self, $x) {
    $self->{pos} += $x;
    my $missing = $self->{pos} - @{$self->{tape}} + 1;
    for (my $i = 0; $i < $missing; $i++) {
        push @{$self->{tape}}, 0;
    }
}


package Printer;

sub new ($class, $quiet) {
    my $self = {
        sum1 => 0,
        sum2 => 0,
        quiet => $quiet
    };
    bless $self, $class;
    return $self;
}

sub print ($self, $n) {
    if ($self->{quiet}) {
        $self->{sum1} = ($self->{sum1} + $n) % 255;
        $self->{sum2} = ($self->{sum2} + $self->{sum1}) % 255;
    } else {
        printf "%c", $n;
    }
}

sub checksum ($self) {
    return ($self->{sum2} << 8) | $self->{sum1};
}


package Main;

my $INC   = 1;
my $MOVE  = 2;
my $PRINT = 3;
my $LOOP  = 4;

sub parse ($source, $i = 0) {
    my $repr = [];
    for (; $i < @{$source}; $i++) {
        given ($source->[$i]) {
            when ('+') { push @$repr, Op->new($INC,   1); }
            when ('-') { push @$repr, Op->new($INC,  -1); }
            when ('>') { push @$repr, Op->new($MOVE,  1); }
            when ('<') { push @$repr, Op->new($MOVE, -1); }
            when ('.') { push @$repr, Op->new($PRINT); }
            when ('[') {
                my ($parsed_loop, $new_i) = parse($source, $i + 1);
                $i = $new_i;
                push @$repr, Op->new($LOOP, $parsed_loop);
            }
            when (']') { last; }
        }
    }
    return ($repr, $i);
}

sub run ($parsed, $tape, $p) {
    foreach my $op (@$parsed) {
        CORE::given ($op->{op}) {
            when ($INC)   { $tape->inc($op->{val}); }
            when ($MOVE)  { $tape->move($op->{val}); }
            when ($PRINT) { $p->print($tape->get()); }
            when ($LOOP)  {
                while ($tape->get() > 0) {
                    run($op->{val}, $tape, $p);
                }
            }
        }
    }
}

sub notify ($msg) {
    socket(my $socket, Socket::PF_INET, Socket::SOCK_STREAM, (getprotobyname('tcp'))[2]);
    if (connect($socket, Socket::pack_sockaddr_in(9001, Socket::inet_aton('localhost')))) {
        print $socket $msg;
    }
    close($socket);
}

sub verify {
    my $text = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
       ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    my $p_left = Printer->new(1);
    my @source = split(//, $text);
    my ($parsed, $n) = parse(\@source);
    run($parsed, Tape->new(), $p_left);
    my $left = $p_left->checksum;

    my $p_right = Printer->new(1);
    foreach my $c (split //, "Hello World!\n") {
        $p_right->print(ord($c));
    }
    my $right = $p_right->checksum;

    if ($left != $right) {
        print STDERR "${left} != ${right}\n";
        exit(1);
    }
}

if ($0 eq __FILE__) {
    verify;
    open (FH, "<", shift) or die $!;
    undef $/;
    $| = 1;
    read FH, my $text, -s FH;
    close(FH);
    my $p = Printer->new($ENV{'QUIET'});

    notify("Perl\t". $$);
    my @source = split(//, $text);
    my ($parsed, $n) = parse(\@source);
    run($parsed, Tape->new(), $p);
    notify("stop");

    if ($p->{quiet}) {
        printf("Output checksum: %d\n", $p->checksum);
    }
}
