package require Tcl 8.6

namespace eval bf {
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

    proc run {program tape pos} {
        foreach x $program {
            lassign $x op val
            switch -exact -- $op {
                INC {
                    lset tape $pos [expr {[lindex $tape $pos] + $val}]
                }
                MOVE {
                    incr pos $val
                    while {$pos >= [llength $tape]} {
                        lappend tape 0
                    }
                }
                PRINT {
                    puts -nonewline [format %c [lindex $tape $pos]]
                    flush stdout
                }
                LOOP {
                    while {[lindex $tape $pos] > 0} {
                        lassign [run $val $tape $pos] tape pos
                    }
                }
            }
        }
        return [list $tape $pos]
    }
}   

proc main text {
    lassign [::bf::parse $text] program
    ::bf::run $program 0 0
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

notify [format "%s\t%d" "Tcl (FP)" [pid]]
main $text
notify "stop"
