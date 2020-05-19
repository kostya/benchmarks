package require Tcl 8.6
package require math::linearalgebra

proc generate n {
    set M {}
    set t [expr { 1.0 / $n / $n }]

    for {set i 0} {$i < $n} {incr i} {
        set v {}
        for {set j 0} {$j < $n} {incr j} {
            lappend v [expr { $t * ($i - $j) * ($i + $j) }]
        }
        lappend M $v
    }
    return $M
}

proc main n {
    set A [generate $n]
    set B $A

    set C [::math::linearalgebra::matmul $A $B]

    set halfN [expr { $n / 2 }]

    puts [lindex $C $halfN $halfN]
}

proc notify msg {
    catch {
        set sock [socket "localhost" 9001]
        puts $sock $msg
        close $sock
    }
}

if {[llength $argv] > 0} {
    set n [expr int([lindex $argv 0] / 2 * 2)]
} else {
    set n 100
}

set t [::math::linearalgebra::matmul [generate 100] [generate 100]]
if {[expr abs([lindex $t 1 1] + 19.5)] > 0.5} {
    exit -1
}

notify [format "%s\t%d" "Tcl" [pid]]

main $n

notify "stop"
