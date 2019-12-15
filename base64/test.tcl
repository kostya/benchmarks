package require Tcl 8.6

set STR_SIZE 131072
set TRIES 8192

proc main {size tries} {
    set str [string repeat a $size]

    set encoded [binary encode base64 $str]
    puts -nonewline "encode [string range $str 0 4]... to [string range $str 0 4]...: "

    set t [clock milliseconds]
    set length 0
    for {set i 0} {$i < $tries} {incr i} {
        set encoded [binary encode base64 $str]
        incr length [string length $encoded]
    }
    puts "$length, [expr { ([clock milliseconds] - $t) / 1000.0 }]"

    set decoded [binary decode base64 $encoded]
    puts -nonewline "decode [string range $encoded 0 4]... to [string range $decoded 0 4]...: "

    set t [clock milliseconds]
    set length 0
    for {set i 0} {$i < $tries} {incr i} {
        set decoded [binary decode base64 $encoded]
        incr length [string length $decoded]
    }
    puts "$length, [expr { ([clock milliseconds] - $t) / 1000.0 }]"
}

catch {
    set sock [socket "localhost" 9001]
    puts $sock "Tcl"
    close $sock
}

main $STR_SIZE $TRIES
