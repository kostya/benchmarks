package require Tcl 8.6

namespace eval bf {} {
    ::oo::class create Tape {
        variable tape pos
        
        constructor {} {
            set tape 0
            set pos 0
        }
        
        method current {} {
            return [lindex $tape $pos]
        }
        
        method inc x {
            lset tape $pos [expr {[lindex $tape $pos] + $x}]
        }
        
        method move x {
            incr pos $x
            while {$pos >= [llength $tape]} {
                lappend tape 0
            }
        }
    }

    proc parse source {
        set res {}
        while 1 {
            set c [lindex $source 0]
            if {$c eq {}} break
            set source [lrange $source 1 end]
            switch -exact -- $c {
                + { lappend res [list INC 1] }
                - { lappend res [list INC -1] }
                > { lappend res [list MOVE 1] }
                < { lappend res [list MOVE -1] }
                . { lappend res [list PRINT {}] }
                \[ {
                    lassign [parse $source] loop_code source
                    lappend res [list LOOP $loop_code]
                }
                \] { break }
                default {}
            }
        }
        return [list $res $source]
    }

    proc run {program tape} {
        foreach x $program {
            lassign $x op val
            switch -exact -- $op {
                INC {
                    $tape inc $val
                }
                MOVE {
                    $tape move $val
                }
                PRINT {
                    puts -nonewline [format %c [$tape current]]
                    flush stdout
                }
                LOOP {
                    while {[$tape current] > 0} {
                        run $val $tape
                    }
                }
            }
        }
    }
}   

proc main text {
    lassign [::bf::parse $text] program
    set tape [::bf::Tape new]
    ::bf::run $program $tape
    $tape destroy
}

proc notify msg {
    catch {
        set sock [socket "localhost" 9001]
        puts $sock $msg
        close $sock
    }
}

lassign $argv filename
set f [open $filename]
set text [split [read $f] {}]
close $f

notify [format "%s\t%d" "Tcl (OO)" [pid]]
main $text
notify "stop"
