package require Tcl 8.6

namespace eval ::brainfuck {
    variable verson 0.2.0
    variable debug 0

    proc emit code {
        upvar 1 transl transl
        foreach line [split [string trim $code] \n] {
            append transl [string trimleft $line]\n
        }
    }

    proc translate source {
        set transl {}
        emit {
            if {![info exists tape] || ($tape eq {})} {
                for {set i 0} {$i < 30000} {incr i} {
                    dict set tape $i 0
                }
            }
            if {![info exists pos] || ($pos eq {})} {
                set pos 0
            }
        }

        set commands [split $source {}]
        for {set i 0} {$i < [llength $commands]} {incr i} {
            switch -exact -- [lindex $commands $i] {
                [ {
                    emit [string cat {if {[dict get $tape $pos] != 0} } \{]
                    emit "while 1 \{"
                }
                ] {
                    emit {if {[dict get $tape $pos] == 0} {break}}
                    emit \}
                    emit \}
                }
                > -
                < {
                    # Compress repeated Brainfuck instructions into a single Tcl
                    # command.
                    if {[lindex $commands $i] eq {<}} {
                        set op <
                        set sign -
                    } else {
                        set op >
                        set sign +
                    }

                    set count 0
                    while {[lindex $commands $i] eq $op} {
                        incr count
                        incr i
                    }
                    incr i -1

                    emit [format {incr pos %s%d} $sign $count]                
                }
                + -
                - {
                    if {[lindex $commands $i] eq {+}} {
                        set op +
                    } else {
                        set op -
                    }

                    set count 0
                    while {[lindex $commands $i] eq $op} {
                        incr count
                        incr i
                    }
                    incr i -1

                    emit [format {dict incr tape $pos %s%d} $op $count]
                }
                . {
                    emit {
                        puts -nonewline [format %c [dict get $tape $pos]]
                    }
                }
                , {
                    # Noncompliant implementation.
                    emit {
                        set input [read stdin 1]
                        dict set tape $pos [scan $input %c]
                    }
                }
                default {
                    # Ignore.
                }
            }
        }
        emit {
            return [list $tape $pos]
        }
        return $transl
    }

    proc interpret source {
        variable debug

        set translated [translate $source]
        if {$debug} {
            puts $translated
        }
        set result [apply [list {{tape {}} {pos {}}} $translated]]
        return $result
    }
}

proc main filename {
    set ch [open $filename r]
    set source [read $ch]
    close $ch

    fconfigure stdout -buffering none

    ::brainfuck::interpret $source
}

main [lindex $argv 0]
