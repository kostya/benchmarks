import std/io
import std/json
import std/store

std/compiler:paths { notify: ./notify.kz }

import notify

std/store:new(st) { x: 0.0[f64], y: 0.0[f64], z: 0.0[f64], n: 0.0[f64] }

pub tor ret0 {} -> i64
ret0 -> 0

pub tor ret1 {} -> i64
ret1 -> 1

pub tor calc { text: string<*> } -> i64
calc = std/store:stored { st.x: 0.0, st.y: 0.0, st.z: 0.0, st.n: 0.0 }
    |> std/json:parse(input: text)
    | malformed { message, offset } |> std/io:print.ln("malformed @{{ offset:d }}: {{ message:s }}")
        |> ret1(): r -> r
    | ok { coordinates: []{ x: f64, y: f64, z: f64 } }
        |> std/store:stored { st.n: @as(f64, @floatFromInt(coordinates.len)) }
        |> for(0..coordinates.len)
            ! each i |> std/store:stored { st.x: st.x + coordinates[i].x, st.y: st.y + coordinates[i].y, st.z: st.z + coordinates[i].z }
        | done |> ret0(): r -> r

pub tor chk { text: string<*>, tag: string } -> i64
chk = calc(text): rc
    |> if(rc != 0 or st.x != 2.0 or st.y != 0.5 or st.z != 0.25)
        | then |> std/io:print.ln("VERIFY FAIL {{ tag:s }}")
            |> ret1(): r -> r
        | else |> ret0(): r -> r

chk(text: "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}", tag: "normal"): r1
|> chk(text: "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}", tag: "reorder"): r2
|> if(r1 != 0 or r2 != 0)
    | then |> std/io:print.ln("VERIFY_FAIL")
    | else |> std/io:read-file(path: "/tmp/1.json")
        | not-found |> std/io:print.ln("file missing")
        | failed _ |> std/io:print.ln("read failed")
        | ok content |> notify:send-pid(name: "Koru", port: 9001): _
            |> calc(text: content): rc
            |> notify:send(msg: "stop", port: 9001): _
            |> if(rc == 0)
                | then |> std/store:stored { st.x: st.x / st.n, st.y: st.y / st.n, st.z: st.z / st.n }
                    |> std/io:print.ln("Coordinate { x: {{ st.x:e }}, y: {{ st.y:e }}, z: {{ st.z:f }} }")
            |> std/io:free(content)
