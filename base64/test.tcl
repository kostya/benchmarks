package require Tcl 8.6

set STR_SIZE 131072
set TRIES 8192
set str [string repeat a $STR_SIZE]

proc main {tries} {
    global str
    set t [clock milliseconds]
    set length 0

    set encoded [binary encode base64 $str]
    puts -nonewline "encode [string range $str 0 4]... to [string range $str 0 4]...: "

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

proc notify msg {
    catch {
        set sock [socket "localhost" 9001]
        puts $sock $msg
        close $sock
    }
}

notify [format "%s\t%d" "Tcl" [pid]]

main $TRIES

notify "stop"
