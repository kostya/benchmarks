open System
open System.Diagnostics
open System.IO

type Op = Inc of int | Move of int | Print | Loop of Op list
type Tape =
    { data : int array;
      pos : int;
  }
type Printer =
    { sum1 : int;
      sum2: int;
      quiet: bool;
  }

let print p n =
    if p.quiet then
        let newSum1 = (p.sum1 + n) % 255
        let newSum2 = (newSum1 + p.sum2) % 255
        { sum1 = newSum1; sum2 = newSum2; quiet = true }
    else
        printf "%c" (char n)
        Console.Out.Flush()
        p

let getChecksum p = (p.sum2 <<< 8) ||| p.sum1

let current t =
    t.data.[t.pos]

let inc delta t =
    t.data.[t.pos] <- t.data.[t.pos] + delta

let move m t =
    let newPos = t.pos + m
    let len = Array.length t.data
    let newData =
        if newPos < len then t.data
        else Array.append t.data (Array.create (newPos - len + 1) 0)
    { data = newData; pos = newPos }

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
            let (newS, loopCode) = parse (rest, [])
            parse (newS, Loop loopCode :: acc)
        | ']' -> (rest, List.rev acc)
        | _ -> parse (rest, acc)

let rec run program t p =
    match program with
    | [] -> (t, p)
    | Inc d :: ops ->
        inc d t
        run ops t p
    | Move m :: ops -> run ops (move m t) p
    | Print :: ops ->
        let newP = t |> current |> print p
        run ops t newP
    | Loop loopCode :: ops ->
        let rec loop (t, p) =
            if current t = 0 then run ops t p
            else run loopCode t p |> loop
        in loop (t, p)

let notify (msg: string) =
    try
        use s = new System.Net.Sockets.TcpClient("localhost", 9001)
        let data = System.Text.Encoding.UTF8.GetBytes(msg)
        ignore(s.Client.Send(data))
    with _ -> ()

let verify =
    let source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
        ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let (_, ops) = parse(source, [])
    let p = { sum1 = 0; sum2 = 0; quiet = true }
    let (_, pLeft) = run ops { data = [| 0 |]; pos = 0 } p
    let left = getChecksum pLeft

    let seq = [for c in "Hello World!\n" -> (int c)]
    let pRight = List.fold print p seq
    let right = getChecksum pRight
    if left <> right then
        Printf.eprintf "%d != %d\n" left right;
        exit 1

[<EntryPoint>]
let main argv =
    verify
    match argv with
    | [| filename |] ->
        let source =
            File.ReadAllLines filename
            |> String.concat "\n"
        let quiet = Environment.GetEnvironmentVariable("QUIET") <> null
        let p = { sum1 = 0; sum2 = 0; quiet = quiet}

        let runtime = if isNull(Type.GetType("Mono.Runtime")) then ".NET Core" else "Mono"
        notify(String.Format("F#/{0}\t{1}", runtime, Process.GetCurrentProcess().Id))
        let stopWatch = Stopwatch.StartNew()

        let (_, ops) = parse(source, [])
        let (_, pNew) = run ops { data = [| 0 |]; pos = 0 } p
        stopWatch.Stop()
        let elapsed = stopWatch.Elapsed.TotalSeconds

        notify("stop")

        Console.Error.WriteLine("time: {0}s", elapsed)
        if pNew.quiet then
            printf "Output checksum: %d\n" (getChecksum pNew)
        0
    | _ -> 1
