package require Tcl 8.6

set STR_SIZE 10000000
set TRIES 100

proc main {size tries} {
    set str [string repeat a $size]

    set t [clock milliseconds]
    set length 0
    for {set i 0} {$i < $tries} {incr i} {
        set encoded [binary encode base64 $str]
        incr length [string length $encoded]
    }
    puts "encode: $length, [expr { ([clock milliseconds] - $t) / 1000.0 }]"

    set t [clock milliseconds]
    set length 0
    for {set i 0} {$i < $tries} {incr i} {
        set decoded [binary decode base64 $encoded]
        incr length [string length $decoded]
    }
    puts "decode: $length, [expr { ([clock milliseconds] - $t) / 1000.0 }]"
}

main $STR_SIZE $TRIES
