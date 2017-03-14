open System
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
      if current t = 0 then run ops t
      else
        run loop_code t |> run program

[<EntryPoint>]
let main argv =
  match argv with
  | [| filename |] ->
    let source =
      File.ReadAllLines filename
      |> String.concat "\n"
    let (_, ops) = parse(source, [])
    let _ = run ops { data = [| 0 |]; pos = 0 }
    0
  | _ -> 1
