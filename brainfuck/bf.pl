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

sub run ($parsed, $tape) {
    foreach my $op (@$parsed) {
        CORE::given ($op->{op}) {
            when ($INC)   { $tape->inc($op->{val}); }
            when ($MOVE)  { $tape->move($op->{val}); }
            when ($PRINT) { printf "%c", $tape->get(); }
            when ($LOOP)  {
                while ($tape->get() > 0) {
                    run($op->{val}, $tape);
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

if ($0 eq __FILE__) {
    open (FH, "<", shift) or die $!;
    undef $/;
    $| = 1;
    my $text = [split //, <FH>];
    close(FH);

    my $pid = $$;
    notify("Perl\t${pid}");

    my ($parsed, $n) = parse($text);
    my $tape = Tape->new();
    run($parsed, $tape);

    notify("stop");
}
