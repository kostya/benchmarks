package require Tcl 8.6

namespace eval ::brainfuck {}

::oo::class create ::brainfuck::Tape {
    constructor {} {
        my variable data
        my variable pos

        set data [lrepeat 40000 0]
        set pos 0
    }

    method get {} {
        my variable data
        my variable pos

        return [lindex $data $pos]
    }

    method set value {
        my variable data
        my variable pos

        lset data $pos $value
    }

    method > {} {
        my variable pos
        incr pos
    }

    method < {} {
        my variable pos
        incr pos -1
    }

    method + {} {
        set value [my get]
        incr value
        my set $value
    }

    method - {} {
        set value [my get]
        incr value -1
        my set $value
    }

    method . {} {
        set value [my get]
        puts -nonewline [format %c $value]
    }

    method , {} {
        # Noncompliant implementation.
        set input [read stdin 1]
        my set [scan $input %c]
    }
}
::oo::define ::brainfuck::Tape export < > + - . ,

::oo::class create ::brainfuck::Interpreter {
    constructor {} {
        my variable tape

        set tape [::brainfuck::Tape new]
    }

    destructor {
        my variable tape

        $tape destroy
    }

    # Return a dictionary that matches each opening bracket to a closing bracket
    # and vice versa.
    method indexBrackets program {
        set length [llength $program]

        set index {}
        set level 0
        set brackets {}
        set i 0
        foreach c $program {
            switch -exact -- $c {
                [ {
                    incr level
                    dict set brackets $level $i 
                }
                ] {
                    set match [dict get $brackets $level]
                    dict set index $i $match
                    dict set index $match $i
                    incr level -1
                }
                default {}
            }
            incr i
        }
        return $index
    }

    method interpret source {
        my variable tape

        set counter 0 ;# Program counter.

        set program [split $source {}]
        set length [llength $program]

        set bracketIndex [my indexBrackets $program]

        while {$counter < $length} {
            set op [lindex $program $counter]
            switch -exact -- $op {
                [ {
                    if {[$tape get] == 0} {
                        set counter [dict get $bracketIndex $counter]
                    }
                }
                ] {
                    if {[$tape get] != 0} {
                        set counter [dict get $bracketIndex $counter]
                    }
                }
                default {
                    if {$op in {< > + - . ,}} {
                        $tape $op
                    }
                }
            }
            incr counter
        }
    }
}

proc main filename {
    set ch [open $filename r]
    set source [read $ch]
    close $ch

    fconfigure stdout -buffering none

    set bf [::brainfuck::Interpreter new]
    $bf interpret $source
    $bf destroy
}

main [lindex $argv 0]
