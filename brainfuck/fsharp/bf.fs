open System
module Tape =
    type t = { mutable data: int array; mutable pos: int }

    let create () = { data = [| 0 |]; pos = 0 }
    let current t = t.data.[t.pos]
    let inc t delta =
        t.data.[t.pos] <- current t + delta

    let put t ch =
        t.data.[t.pos] <- ch
        
    let move t m =
        let newPos = t.pos + m
        if newPos >= Array.length t.data then
            Array.Resize (&t.data, 2 * newPos)
        t.pos <- newPos

module Printer =
    type sum =
        { mutable sum1: int
          mutable sum2: int }
    
    [<Struct>]
    type t =
        | Quiet of sum
        | Stdout

    let createQuiet () = Quiet { sum1 = 0; sum2 = 0 }
    let createStdout () = Stdout

    let print p n =
        match p with
        | Stdout ->
            Console.Out.Write(char n)
            Console.Out.Flush()
        | Quiet p ->
            let newSum1 = (p.sum1 + n) % 255
            let newSum2 = (newSum1 + p.sum2) % 255
            p.sum1 <- newSum1
            p.sum2 <- newSum2
        p
    let checksum = function
        | Stdout -> 0
        | Quiet sm -> (sm.sum2 <<< 8) ||| sm.sum1

    let isQuiet = function
        | Quiet _ -> true
        | _ -> false

module Interpreter =
    
    [<Struct>]
    type Op =
        | Inc of delta: int
        | Move of steps: int
        | Print
        | Input
        | Loop of opcodes: List<Op>
        | Ignore

    let parse (s: string) =
        let opCodes = Set.ofSeq "+-<>,."
        let rec _parse res = function
            | [] -> List.rev res, List.empty
            | ']' :: tail -> List.rev res, tail
            | '[' :: tail ->
                let (codes, newTail) = _parse [] tail
                _parse (Loop codes :: res) newTail
            | ch :: tail when Set.contains ch opCodes  ->
                let code =
                    match ch with
                    | '+' -> Inc 1
                    | '-' -> Inc -1
                    | '>' -> Move 1
                    | '<' -> Move -1
                    | '.' -> Print
                    | ',' -> Input
                    | _ -> Ignore
                _parse (code :: res) tail
            | _ :: tail -> _parse res tail            
        Seq.toList s
            |> _parse []
            |> fst

    let rec run tape printer program = 
        let rec _run = function
            | [] -> ()
            | Inc delta :: restProgram ->
                Tape.inc tape delta
                _run  restProgram
            | Move delta :: rest ->
                Tape.move tape delta
                _run rest
            | Print :: rest ->
                Tape.current tape |> Printer.print printer |> ignore
                _run rest
            | Loop loopCode :: rest ->
                    while Tape.current tape > 0 do
                        _run loopCode
                    _run rest
            | Input :: rest ->
                let c = Console.Read()
                Tape.put tape c
                _run rest
            | Ignore :: rest -> _run rest
        _run program
    
    let runWithNewTape printer program =
        run (Tape.create ()) printer program
        printer

open System.Diagnostics
open System.IO
module Util =
    let notify (msg: string) =
        try
            use s = new System.Net.Sockets.TcpClient("localhost", 9001)
            msg
                |> System.Text.Encoding.UTF8.GetBytes
                |> s.Client.Send
                |> ignore
        with _ -> ()

    let verify () =
        let source = """
            ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
            ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
            """
        let left =
            Interpreter.parse source
            |> Interpreter.runWithNewTape (Printer.createQuiet ())
            |> Printer.checksum

        let right =
            "Hello World!\n"
            |> Seq.map int
            |> Seq.fold Printer.print (Printer.createQuiet())
            |> Printer.checksum

        if left <> right
        then Result.Error <| sprintf "%d != %d" left right 
        else Result.Ok None

    let isQuiet =
        Environment.GetEnvironmentVariable "QUIET"
        <> null

    let runtimeId =
        let runtime = if isNull (Type.GetType "Mono.Runtime") then ".NET Core" else "Mono"
        sprintf "F#/%s\t%d" runtime (Process.GetCurrentProcess().Id)

[<EntryPoint>]
let main argv =
    match Util.verify () with
    | Error error ->
        printfn "%A" error
        exit 1
    | Ok _ -> ()

    match argv with
    | [| filename |] ->
        let source =
            try
                File.ReadAllText filename
            with ex ->
                printfn "Can't open file '%O': %A" filename ex.Message
                exit 1

        let printer = if Util.isQuiet then Printer.createQuiet () else Printer.createStdout ()

        Util.notify Util.runtimeId

        let stopWatch = Stopwatch.StartNew()

        let pNew =
            Interpreter.parse source
            |> Interpreter.runWithNewTape printer

        stopWatch.Stop()

        let elapsed = stopWatch.Elapsed.TotalSeconds

        Util.notify "stop"
        printfn "time: %Os" elapsed

        if Printer.isQuiet pNew then
            Printer.checksum pNew |> printfn "Output checksum: %d"
        0
    | _ ->
        printfn "usage: bf.fs <brainfuck.b>"
        1
