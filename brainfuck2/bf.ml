type op = Inc of int | Move of int | Print | Loop of op list
type tape =
    { data : int array;
      pos : int;
    }

let current t =
  t.data.(t.pos)

let inc delta t =
  t.data.(t.pos) <- t.data.(t.pos) + delta

let move m t =
  let new_pos = t.pos + m in
  let len = Array.length t.data in
  let new_data =
    if new_pos < len then t.data
    else Array.append t.data (Array.make (new_pos - len + 1) 0) in
  { data = new_data; pos = new_pos }

let rec parse (s, acc) =
  if s = "" then ("", List.rev acc)
  else
    let c = s.[0] in
    let rest = String.sub s 1 ((String.length s) - 1) in
    match c with
    | '+' -> parse (rest, Inc 1 :: acc)
    | '-' -> parse (rest, Inc ~-1 :: acc)
    | '>' -> parse (rest, Move 1 :: acc)
    | '<' -> parse (rest, Move ~-1 :: acc)
    | '.' -> parse (rest, Print :: acc)
    | '[' ->
        let (new_s, loop_code) = parse (rest, []) in
        parse (new_s, Loop loop_code :: acc)
    | ']' -> (rest, List.rev acc)
    | _ -> parse (rest, acc)

let rec run program t =
  match program with
  | [] -> t
  | Inc d :: ops ->
      inc d t;
      run ops t
  | Move m :: ops -> run ops (move m t)
  | Print :: ops ->
      print_char (Char.chr (current t));
      flush stdout;
      run ops t
  | Loop loop_code :: ops ->
     let rec loop t =
       if current t = 0 then run ops t
       else loop (run loop_code t)
     in loop t

let read_file filename =
  let s = ref "" in
  let chan = open_in filename in
  try
    while true; do
      s := !s ^ input_line chan
    done;
    !s
  with End_of_file ->
    close_in chan;
    !s

let main =
  let _ =
    let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 9001) in
    try
      let (_, oc) = Unix.open_connection(addr) in
      Fun.protect (fun() -> output_string oc "OCaml")
        ~finally:(fun() -> close_out oc)
    with Unix.Unix_error _ -> () in

  match Sys.argv with
  | [| _; filename |] ->
    let source = read_file filename in
    let (_, ops) = parse(source, []) in
    run ops { data = [| 0 |]; pos = 0 }
  | _ -> exit 1
