import std/io
import std/args
import std/string
import std/grid
import std/store

std/compiler:paths { notify: ./notify.kz }

import notify

std/grid:new(a, size: 2250000) { v: 0.0[f64] }
std/grid:new(b, size: 2250000) { v: 0.0[f64] }
std/grid:new(c, size: 2250000) { v: 0.0[f64] }

std/store:new(st) { n: 0[i64] }

pub tor gena { n: usize }
gena = for(0..n)
    ! each i |> for(0..n)
        ! each j |> std/grid:stored {
            a[i * n + j].v: 1.0 / @as(f64, @floatFromInt(n)) / @as(f64, @floatFromInt(n)) * @as(f64, @floatFromInt(@as(i64, @intCast(i)) - @as(i64, @intCast(j)))) * @as(f64, @floatFromInt(@as(i64, @intCast(i)) + @as(i64, @intCast(j))))
        }

pub tor genb { n: usize }
genb = for(0..n)
    ! each i |> for(0..n)
        ! each j |> std/grid:stored {
            b[i * n + j].v: 2.0 / @as(f64, @floatFromInt(n)) / @as(f64, @floatFromInt(n)) * @as(f64, @floatFromInt(@as(i64, @intCast(i)) - @as(i64, @intCast(j)))) * @as(f64, @floatFromInt(@as(i64, @intCast(i)) + @as(i64, @intCast(j))))
        }

pub tor cz { n: usize }
cz = for(0..n)
    ! each i |> for(0..n)
        ! each j |> std/grid:stored { c[i * n + j].v: 0.0 }

pub tor mul { n: usize }
mul = for(0..n)
    ! each i |> for(0..n)
        ! each k |> capture { aik: a[i * n + k].v }
            ! as ai |> for(0..n)
                ! each j |> std/grid:stored {
                    c[i * n + j].v: c[i * n + j].v + ai.aik * b[k * n + j].v
                }

pub tor idv { v: f64 } -> f64
idv -> v

pub tor prep { n: usize }
prep = gena(n) |> genb(n) |> cz(n)

pub tor calc { n: usize } -> f64
calc = prep(n: @divTrunc(n, 2) * 2) |> mul(n: @divTrunc(n, 2) * 2) |> idv(v: c[@divTrunc(n, 2) * @divTrunc(n, 2) * 2 + @divTrunc(n, 2)].v): r -> r

std/args:get(index: 1)
| arg a |> std/string:parse-int(s: a)
    | ok nv |> if(nv < 0)
        | then |> std/store:stored { st.n: 100 }
        | else |> std/store:stored { st.n: nv }
    | err _ |> std/store:stored { st.n: 100 }
| out-of-bounds |> std/store:stored { st.n: 100 }
|> calc(n: 101): vchk
|> if(@abs(vchk + 18.67) > 0.1)
    | then |> std/io:print.ln("VERIFY_FAIL")
    | else |> if(st.n > 1500)
        | then |> std/io:print.ln("BOUND_FAIL")
        | else |> notify:send-pid(name: "Koru", port: 9001): _
            |> calc(n: @as(usize, @intCast(st.n))): res
            |> notify:send(msg: "stop", port: 9001): _
            |> std/io:print.ln("{{ res:f }}")
