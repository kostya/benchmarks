package require Tcl 8.6
package require math::linearalgebra

proc generate {n seed} {
    set M {}
    set t [expr { $seed / $n / $n }]

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
    set A [generate $size 1.0]
    set B [generate $size 2.0]

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

apply {{{n 100}} {
    set left [calc 101]
    set right -18.67
    if {[expr abs($left - $right)] > 0.1} {
        puts stderr [format "%f != %f" $left $right]
        exit 1
    }

    notify [format "%s\t%d" "Tcl" [pid]]
    set results [calc $n]
    notify "stop"

    puts results
}} {*}$argv
