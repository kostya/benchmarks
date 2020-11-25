package require Tcl 8.6

namespace eval bf {
    ::oo::class create Printer {
        variable sum1 sum2 quiet

        constructor q {
            set sum1 0
            set sum2 0
            set quiet $q
        }

        method print n {
            if {$quiet} {
                set sum1 [expr ($sum1 + $n) % 255]
                set sum2 [expr ($sum2 + $sum1) % 255]
            } else {
                puts -nonewline [format %c $n]
                flush stdout
            }
        }

        method checksum {} {
            return [expr ($sum2 << 8) | $sum1]
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

    proc run {program tape pos p} {
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
                    $p print [lindex $tape $pos]
                }
                LOOP {
                    while {[lindex $tape $pos] > 0} {
                        lassign [run $val $tape $pos $p] tape pos
                    }
                }
            }
        }
        return [list $tape $pos]
    }
}

proc main {text p} {
    lassign [::bf::parse [split $text {}]] program
    ::bf::run $program 0 0 $p
}

proc notify msg {
    catch {
        set sock [socket "localhost" 9001]
        puts $sock $msg
        close $sock
    }
}


proc verify {} {
    set text {++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.}
    set p_left [::bf::Printer new 1]
    main $text $p_left
    set left [$p_left checksum]
    $p_left destroy

    set p_right [::bf::Printer new 1]
    foreach c [split "Hello World!\n" ""] {
        $p_right print [scan $c %c]
    }
    set right [$p_right checksum]
    $p_right destroy
    if {$left != $right} {
        puts stderr [format "%d != %d" $left $right]
        exit 1
    }
}

apply {{filename} {
    verify
    set f [open $filename]
    set text [read $f]
    close $f
    set quiet [info exists ::env(QUIET)]
    set p [::bf::Printer new $quiet]

    notify [format "%s\t%d" "Tcl (FP)" [pid]]
    main $text $p
    notify "stop"

    if {$quiet} {
        puts [format "Output checksum: %d" [$p checksum]]
    }
    $p destroy
}} {*}$argv
