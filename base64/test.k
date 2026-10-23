import std/io
import std/grid
import std/store

std/compiler:paths { notify: ./notify.kz }

import notify

pub tor idv8 { v: u8 } -> u8
idv8 -> v

pub tor idv { v: i64 } -> i64
idv -> v

pub tor ret0 {} -> i64
ret0 -> 0

pub tor b64ch { v: u8 } -> u8
b64ch = idv8(v: if (v == 62) @as(u8, 43) else @as(u8, 47)): r3
    |> idv8(v: if (v < 62) v -% 4 else r3): r2
    |> idv8(v: if (v < 52) v +% 71 else r2): r1
    |> idv8(v: if (v < 26) v +% 65 else r1): r -> r

pub tor value { c: u8 } -> u8
value = idv8(v: if (c == 47) @as(u8, 63) else @as(u8, 255)): r4
    |> idv8(v: if (c == 43) @as(u8, 62) else r4): r3
    |> idv8(v: if (c >= 48 and c < 58) c +% 4 else r3): r2
    |> idv8(v: if (c >= 65 and c < 91) c -% 65 else r2): r1
    |> idv8(v: if (c >= 97 and c < 123) c -% 71 else r1): r -> r

pub tor enc1 {} -> i64
enc1 = std/grid:walk { s: inp.b, 3, d: enc.c, 4 }
    ! each |> b64ch(v: @divTrunc(s[0], 4)): c0
        |> b64ch(v: @rem(s[0], 4) *% 16 +% @divTrunc(s[1], 16)): c1
        |> b64ch(v: @rem(s[1], 16) *% 4 +% @divTrunc(s[2], 64)): c2
        |> b64ch(v: @rem(s[2], 64)): c3
        |> std/grid:stored { d[0]: c0, d[1]: c1, d[2]: c2, d[3]: c3 }
    | tail |> b64ch(v: @divTrunc(s[0], 4)): t0
        |> b64ch(v: @rem(s[0], 4) *% 16 +% @divTrunc(s[1], 16)): t1
        |> b64ch(v: @rem(s[1], 16) *% 4): t2
        |> std/grid:stored { d[0]: t0, d[1]: t1, d[2]: t2, d[3]: 61 }
    | done |> ret0(): _ -> 174764

pub tor dec1 {} -> i64
dec1 = std/grid:walk { s: enc.c, 4, d: dec.b, 3 }
    ! each |> value(c: s[0]): v0
        |> value(c: s[1]): v1
        |> value(c: s[2]): v2
        |> value(c: s[3]): v3
        |> std/grid:stored { d[0]: v0 *% 4 +% @divTrunc(v1, 16), d[1]: v1 *% 16 +% @divTrunc(v2, 4), d[2]: v2 *% 64 +% v3 }
    | tail |> value(c: s[0]): v0
        |> value(c: s[1]): v1
        |> value(c: s[2]): v2
        |> std/grid:stored { d[0]: v0 *% 4 +% @divTrunc(v1, 16), d[1]: v1 *% 16 +% @divTrunc(v2, 4) }
    | done |> ret0(): _ -> 131072

pub tor vcmp {} -> i64
vcmp = std/grid:walk { s: dec.b, 16, t: inp.b, 16 }
    ! each |> std/store:stored { st.bad: st.bad + @as(i64, @intFromBool(s[0] != t[0])) + @as(i64, @intFromBool(s[1] != t[1])) + @as(i64, @intFromBool(s[2] != t[2])) + @as(i64, @intFromBool(s[3] != t[3])) + @as(i64, @intFromBool(s[4] != t[4])) + @as(i64, @intFromBool(s[5] != t[5])) + @as(i64, @intFromBool(s[6] != t[6])) + @as(i64, @intFromBool(s[7] != t[7])) + @as(i64, @intFromBool(s[8] != t[8])) + @as(i64, @intFromBool(s[9] != t[9])) + @as(i64, @intFromBool(s[10] != t[10])) + @as(i64, @intFromBool(s[11] != t[11])) + @as(i64, @intFromBool(s[12] != t[12])) + @as(i64, @intFromBool(s[13] != t[13])) + @as(i64, @intFromBool(s[14] != t[14])) + @as(i64, @intFromBool(s[15] != t[15])) }
    | tail |> for(0..s.len)
        ! each i |> if(s[i] != t[i])
            | then |> std/store:stored { st.bad: st.bad + 1 }
    | done |> ret0(): _ -> 0

pub tor fixtures {} -> i64
fixtures = b64ch(v: @as(u8, @divTrunc(104, 4))): e0
    |> b64ch(v: @as(u8, @rem(104, 4) *% 16 +% @divTrunc(101, 16))): e1
    |> b64ch(v: @as(u8, @rem(101, 16) *% 4 +% @divTrunc(108, 64))): e2
    |> b64ch(v: @as(u8, @rem(108, 64))): e3
    |> b64ch(v: @as(u8, @divTrunc(108, 4))): e4
    |> b64ch(v: @as(u8, @rem(108, 4) *% 16 +% @divTrunc(111, 16))): e5
    |> b64ch(v: @as(u8, @rem(111, 16) *% 4)): e6
    |> b64ch(v: @as(u8, @divTrunc(119, 4))): e7
    |> b64ch(v: @as(u8, @rem(119, 4) *% 16 +% @divTrunc(111, 16))): e8
    |> b64ch(v: @as(u8, @rem(111, 16) *% 4 +% @divTrunc(114, 64))): e9
    |> b64ch(v: @as(u8, @rem(114, 64))): e10
    |> b64ch(v: @as(u8, @divTrunc(108, 4))): e11
    |> b64ch(v: @as(u8, @rem(108, 4) *% 16 +% @divTrunc(100, 16))): e12
    |> b64ch(v: @as(u8, @rem(100, 16) *% 4)): e13
    |> value(c: 97): d0v0
    |> value(c: 71): d0v1
    |> idv8(v: d0v0 *% 4 +% @divTrunc(d0v1, 16)): d0
    |> value(c: 71): d1v1
    |> value(c: 86): d1v2
    |> idv8(v: d1v1 *% 16 +% @divTrunc(d1v2, 4)): d1
    |> value(c: 86): d2v2
    |> value(c: 115): d2v3
    |> idv8(v: d2v2 *% 64 +% d2v3): d2
    |> value(c: 98): d3v0
    |> value(c: 71): d3v1
    |> idv8(v: d3v0 *% 4 +% @divTrunc(d3v1, 16)): d3
    |> value(c: 71): d4v1
    |> value(c: 56): d4v2
    |> idv8(v: d4v1 *% 16 +% @divTrunc(d4v2, 4)): d4
    |> value(c: 100): d5v0
    |> value(c: 50): d5v1
    |> idv8(v: d5v0 *% 4 +% @divTrunc(d5v1, 16)): d5
    |> value(c: 50): d6v1
    |> value(c: 57): d6v2
    |> idv8(v: d6v1 *% 16 +% @divTrunc(d6v2, 4)): d6
    |> value(c: 57): d7v2
    |> value(c: 121): d7v3
    |> idv8(v: d7v2 *% 64 +% d7v3): d7
    |> value(c: 98): d8v0
    |> value(c: 71): d8v1
    |> idv8(v: d8v0 *% 4 +% @divTrunc(d8v1, 16)): d8
    |> value(c: 71): d9v1
    |> value(c: 81): d9v2
    |> idv8(v: d9v1 *% 16 +% @divTrunc(d9v2, 4)): d9
    |> idv(v: @as(i64, @intFromBool(e0 != 97))
    + @as(i64, @intFromBool(e1 != 71))
    + @as(i64, @intFromBool(e2 != 86))
    + @as(i64, @intFromBool(e3 != 115))
    + @as(i64, @intFromBool(e4 != 98))
    + @as(i64, @intFromBool(e5 != 71))
    + @as(i64, @intFromBool(e6 != 56))
    + @as(i64, @intFromBool(e7 != 100))
    + @as(i64, @intFromBool(e8 != 50))
    + @as(i64, @intFromBool(e9 != 57))
    + @as(i64, @intFromBool(e10 != 121))
    + @as(i64, @intFromBool(e11 != 98))
    + @as(i64, @intFromBool(e12 != 71))
    + @as(i64, @intFromBool(e13 != 81))
    + @as(i64, @intFromBool(d0 != 104))
    + @as(i64, @intFromBool(d1 != 101))
    + @as(i64, @intFromBool(d2 != 108))
    + @as(i64, @intFromBool(d3 != 108))
    + @as(i64, @intFromBool(d4 != 111))
    + @as(i64, @intFromBool(d5 != 119))
    + @as(i64, @intFromBool(d6 != 111))
    + @as(i64, @intFromBool(d7 != 114))
    + @as(i64, @intFromBool(d8 != 108))
    + @as(i64, @intFromBool(d9 != 100))): bad -> bad

std/grid:new(inp, size: 131072) { b: 97[u8] }
std/grid:new(enc, size: 174764) { c: 0[u8] }
std/grid:new(dec, size: 131072) { b: 0[u8] }
std/store:new(st) { bad: 0[i64], probe: 0[i64], probe2: 0[i64] }

fixtures(): fbad
|> enc1(): _
|> dec1(): _
|> vcmp(): _
|> idv(v: fbad + st.bad): gate
|> if(gate > 0)
    | then |> std/io:print.ln("FIXTURE_OR_SETUP_FAIL")
    | else |> notify:send-pid(name: "Koru", port: 9001): _
        |> notify:now(): t0
        |> capture { acc: 0[i64] }
            ! as g |> for(0..8192)
                ! each it |> enc1(): l
                    |> std/store:stored { st.probe: st.probe + @as(i64, enc[@rem(it * 16127, 174764)].c) }
                    |> captured { acc: g.acc + l }
            | captured res
                |> notify:now(): t1
                |> capture { acc: 0[i64] }
                    ! as g2 |> for(0..8192)
                        ! each it |> dec1(): l
                            |> std/store:stored { st.probe2: st.probe2 + @as(i64, dec[@rem(it * 16127, 131072)].b) }
                            |> captured { acc: g2.acc + l }
                    | captured res2
                        |> notify:now(): t2
                        |> idv(v: t1 - t0): t1t0
                        |> idv(v: t2 - t1): t2t1
                        |> notify:send(msg: "stop", port: 9001): _
                        |> vcmp(): _
                        |> if(st.bad > 0)
                            | then |> std/io:print.ln("BADROUNDTRIP")
                        |> if(st.probe != 716800 or st.probe2 != 794624)
                            | then |> std/io:print.ln("BADPROBE")
                        |> std/io:print.ln("encode: aaaa... to YWFh...: {{ res.acc:d }}, {{ t1t0:d }}ms")
                        |> std/io:print.ln("decode: YWFh... to aaaa...: {{ res2.acc:d }}, {{ t2t1:d }}ms")
