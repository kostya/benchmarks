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

proc calc n {
    set size [expr int($n / 2) * 2]
    set A [generate $size]
    set B [generate $size]

    set C [::math::linearalgebra::matmul $A $B]

    set halfN [expr { $size / 2 }]

    return [lindex $C $halfN $halfN]
}

proc notify msg {
    catch {
        set sock [socket "localhost" 9001]
        puts $sock $msg
        close $sock
    }
}

set n [expr [llength $argv] > 0 ? [lindex $argv 0] : 100]

set left [calc 101]
set right -9.34
if {[expr abs($left - $right)] > 0.1} {
    puts stderr [format "%f != %f" $left $right]
    exit 1
}

notify [format "%s\t%d" "Tcl" [pid]]

puts [calc $n]

notify "stop"
