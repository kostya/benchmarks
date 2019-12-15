#! /usr/bin/env perl

use 5.10.1;
use strict;
use warnings;
use Socket;
no warnings 'experimental';

package Op;

sub new {
    my $class = shift;
    my $op = shift;
    my $val = shift;

    my $self = {
        op => $op,
        val => $val,
    };
    bless $self, $class;
    return $self;
}


package Tape;

sub new {
    my $class = shift;

    my $self = {
        tape => [0],
        pos => 0,
    };
    bless $self, $class;
    return $self;
}

sub get {
    my $self = shift;

    return $self->{tape}[$self->{pos}];
}

sub inc {
    my $self = shift;
    my $x = shift;

    $self->{tape}[$self->{pos}] += $x;
}

sub move {
    my $self = shift;
    my $x = shift;

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

sub parse {
    my $source = shift;
    my $i = shift || 0;

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

sub run {
    my $parsed = shift;
    my $tape = shift;
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

socket(my $socket, Socket::PF_INET, Socket::SOCK_STREAM, (getprotobyname('tcp'))[2]);
if (connect($socket, Socket::pack_sockaddr_in(9001, Socket::inet_aton('localhost')))) {
  print $socket "Perl";
}
close($socket);

open (FH, "<", shift) or die $!;
undef $/;
$| = 1;
my ($parsed, $n) = parse([split //, <FH>]);
my $tape = Tape->new();
run($parsed, $tape);
