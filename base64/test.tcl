package require Tcl 8.6

proc main {} {
    foreach fixture {{"hello" "aGVsbG8="} {"world" "d29ybGQ="}} {
        set src [lindex $fixture 0]
        set dst [lindex $fixture 1]
        set encoded [binary encode base64 $src]
        if {$encoded != $dst} {
            puts stderr [format "%s != %s" $encoded $dst]
            exit 1
        }
        set decoded [binary decode base64 $dst]
        if {$decoded != $src} {
            puts stderr [format "%s != %s" $decoded $src]
            exit 1
        }
    }

    set STR_SIZE 131072
    set TRIES 8192

    set str [string repeat a $STR_SIZE]
    set encoded [binary encode base64 $str]
    set decoded [binary decode base64 $encoded]

    notify [format "%s\t%d" "Tcl" [pid]]

    set t [clock milliseconds]
    set s_encoded 0
    for {set i 0} {$i < $TRIES} {incr i} {
        incr s_encoded [string length [binary encode base64 $str]]
    }
    set t_encoded [expr { ([clock milliseconds] - $t) / 1000.0 }]

    set t [clock milliseconds]
    set s_decoded 0
    for {set i 0} {$i < $TRIES} {incr i} {
        incr s_decoded [string length [binary decode base64 $encoded]]
    }
    set t_decoded [expr { ([clock milliseconds] - $t) / 1000.0 }]

    notify "stop"

    puts "encode [string range $str 0 4]... to [string range $str 0 4]...: $s_encoded, $t_encoded"
    puts "decode [string range $encoded 0 4]... to [string range $decoded 0 4]...: $s_decoded, $t_decoded"
}

proc notify msg {
    catch {
        set sock [socket "localhost" 9001]
        puts $sock $msg
        close $sock
    }
}

apply {{} {
    main
}}
