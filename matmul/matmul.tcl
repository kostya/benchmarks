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

proc main {{n 100}} {
    set A [generate $n]
    set B $A

    set C [::math::linearalgebra::matmul $A $B]

    set halfN [expr { $n / 2 }]

    puts [lindex $C $halfN $halfN]
}

main [lindex $argv 0]
