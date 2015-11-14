package require Tcl 8.6

namespace eval ::brainfuck {}

::oo::class create ::brainfuck::Interpreter {
    constructor {} {
        my variable tape
        my variable pos

        set tape [lrepeat 30000 0]
        set pos 0
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
        my variable pos

        set counter 0 ;# Program counter.

        set program [split $source {}]
        set length [llength $program]

        set bracketIndex [my indexBrackets $program]

        while {$counter < $length} {
            set op [lindex $program $counter]
            switch -exact -- $op {
                [ {
                    if {[lindex $tape $pos] == 0} {
                        set counter [dict get $bracketIndex $counter]
                    }
                }
                ] {
                    if {[lindex $tape $pos] != 0} {
                        set counter [dict get $bracketIndex $counter]
                    }
                }
                > {
                    incr pos
                }
                < {
                    incr pos -1
                }
                + {
                    set value [lindex $tape $pos]
                    incr value
                    lset tape $pos $value
                }
                - {
                    set value [lindex $tape $pos]
                    incr value -1
                    lset tape $pos $value
                }
                . {
                    set value [lindex $tape $pos]
                    puts -nonewline [format %c $value]
                }
                , {
                    # Noncompliant implementation.
                    set input [read stdin 1]
                    lset tape $pos [scan $input %c]
                }
                default {
                    # Ignore.
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
