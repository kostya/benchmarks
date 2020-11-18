type op = Inc of int | Move of int | Print | Loop of op list
type tape =
  { data : int array;
    pos : int;
  }
type printer =
  { sum1 : int;
    sum2: int;
    quiet: bool;
  }

let print p n =
  if p.quiet then
    let newP = { p with sum1 = (p.sum1 + n) mod 255 } in
    { newP with sum2 = (newP.sum1 + newP.sum2) mod 255}
  else begin
      print_char (Char.chr n);
      flush stdout;
      p
    end

let get_checksum p = (p.sum2 lsl 8) lor p.sum1

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

let rec run program t p =
  match program with
  | [] -> (t, p)
  | Inc d :: ops ->
     inc d t;
     run ops t p
  | Move m :: ops -> run ops (move m t) p
  | Print :: ops ->
     let newP = print p (current t) in
     run ops t newP
  | Loop loop_code :: ops ->
     let rec loop (t, p) =
       if current t = 0 then run ops t p
       else loop (run loop_code t p)
     in loop (t, p)

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

let notify msg =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 9001) in
  try
    let (_, oc) = Unix.open_connection(addr) in
    Fun.protect (fun() -> output_string oc msg)
      ~finally:(fun() -> close_out oc)
  with Unix.Unix_error _ -> ()

let verify =
  let source = {|++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
                ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.|} in
  let (_, ops) = parse(source, []) in
  let p = { sum1 = 0; sum2 = 0; quiet = true } in
  let (_, pLeft) = run ops { data = [| 0 |]; pos = 0 } p in
  let left = get_checksum pLeft in

  let s = "Hello World!\n" in
  let seq = List.init
              (String.length s)
              (fun i -> (Char.code (String.get s i))) in
  let pRight = List.fold_left print p seq in
  let right = get_checksum pRight in
  if left != right then begin
      Printf.eprintf "%d != %d\n" left right;
      exit 1
    end

let main =
  verify;
  match Sys.argv with
  | [| _; filename |] ->
     let source = read_file filename in
     let quiet = Sys.getenv_opt "QUIET" in
     let p = { sum1 = 0; sum2 = 0; quiet = Option.is_some quiet} in

     notify(Printf.sprintf "OCaml\t%d" (Unix.getpid()));
     let (_, ops) = parse(source, []) in
     let (_, newP) = run ops { data = [| 0 |]; pos = 0 } p in
     notify "stop";

     if newP.quiet then
       Printf.printf "Output checksum: %d\n" (get_checksum newP)
  | _ -> exit 1
