open System
open System.Diagnostics
open System.IO

type op = Inc of int | Move of int | Print | Loop of op list
type tape =
    { data : int array;
      pos : int;
    }

let current t =
  t.data.[t.pos]

let inc delta t =
  t.data.[t.pos] <- t.data.[t.pos] + delta

let move m t =
  let new_pos = t.pos + m
  let len = Array.length t.data
  let new_data =
    if new_pos < len then t.data
    else Array.append t.data (Array.create (new_pos - len + 1) 0)
  { data = new_data; pos = new_pos }

let rec parse (s, acc) =
  if s = "" then ("", List.rev acc)
  else
    let c = s.[0]
    let rest = s.[1..((String.length s) - 1)]
    match c with
    | '+' -> parse (rest, Inc 1 :: acc)
    | '-' -> parse (rest, Inc -1 :: acc)
    | '>' -> parse (rest, Move 1 :: acc)
    | '<' -> parse (rest, Move -1 :: acc)
    | '.' -> parse (rest, Print :: acc)
    | '[' ->
        let (new_s, loop_code) = parse (rest, [])
        parse (new_s, Loop loop_code :: acc)
    | ']' -> (rest, List.rev acc)
    | _ -> parse (rest, acc)

let rec run program t =
  match program with
  | [] -> t
  | Inc d :: ops ->
      inc d t
      run ops t
  | Move m :: ops -> run ops (move m t)
  | Print :: ops ->
      t |> current |> char |> printf "%c"
      Console.Out.Flush()
      run ops t
  | Loop loop_code :: ops ->
      let rec loop t =
          if current t = 0 then run ops t
          else run loop_code t |> loop
      in loop t

[<EntryPoint>]
let main argv =
  match argv with
  | [| filename |] ->
    let source =
      File.ReadAllLines filename
      |> String.concat "\n"

    let stopWatch = new Stopwatch()
    Console.Error.WriteLine("JIT warming up")

    stopWatch.Start();
    let (_, ops) = parse(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++", [])
    let _ = run ops { data = [| 0 |]; pos = 0 }
    stopWatch.Stop()
    Console.Error.WriteLine("time: {0}s", stopWatch.Elapsed.TotalSeconds)

    Console.Error.WriteLine("run")
    try
       use s = new System.Net.Sockets.TcpClient("localhost", 9001)
       let runtime = if isNull(Type.GetType("Mono.Runtime")) then ".NET Core" else "Mono"
       let data = System.Text.Encoding.UTF8.GetBytes("F# " + runtime)
       ignore(s.Client.Send(data))
    with _ -> ()

    stopWatch.Restart()
    let (_, ops) = parse(source, [])
    let _ = run ops { data = [| 0 |]; pos = 0 }
    stopWatch.Stop()
    Console.Error.WriteLine("time: {0}s", stopWatch.Elapsed.TotalSeconds)
    0
  | _ -> 1
