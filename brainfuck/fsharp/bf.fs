open System
module Tape =

    [<Struct>]
    type t = { data: int array; pos: int }

    let create () = { data = [| 0 |]; pos = 0 }
    let current t = t.data.[t.pos]
    let inc t delta =
        t.data.[t.pos] <- t.data.[t.pos] + delta
        t

    let move t m =
        let newPos = t.pos + m
        let arrayLen = Array.length t.data
        if newPos >= arrayLen then
            let newData = Array.append t.data (Array.create (2 * arrayLen) 0)
            { data = newData; pos = newPos }
        else
            { t with pos = newPos }

module Printer =
    [<Struct>]
    type sum =
        { sum1: int
          sum2: int }
    
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
            p
        | Quiet p ->
            let newSum1 = (p.sum1 + n) % 255
            let newSum2 = (newSum1 + p.sum2) % 255
            Quiet { sum1 = newSum1; sum2 = newSum2 }
            
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
        | Comment 

    let parse (s: string) =
        let rec _parse res s =
            match s with
            | [] -> List.rev res, List.empty
            | ']' :: tail -> List.rev res, tail
            | '[' :: tail ->
                let (codes, newTail) = _parse [] tail
                _parse (Loop codes :: res) newTail
            | ch :: tail ->
                let code =
                    match ch with
                    | '+' -> Inc 1
                    | '-' -> Inc -1
                    | '>' -> Move 1
                    | '<' -> Move -1
                    | '.' -> Print
                    | _ -> Comment // | ',' -> Input
                _parse (code :: res) tail
        Seq.toList s
            |> _parse []
            |> fst

    let rec run tape printer program = 
        let rec _run (tape, printer) = function
            | [] -> (tape, printer)
            | Inc delta :: restProgram ->
                _run ((Tape.inc tape delta), printer) restProgram
            | Move delta :: rest ->
                _run ((Tape.move tape delta), printer) rest
            | Print :: rest ->
                let newP = Tape.current tape |> Printer.print printer
                _run (tape, newP) rest
            | Loop loopCode :: rest ->
                let rec loop (tape, printer)  =
                    if Tape.current tape = 0 then
                        _run (tape, printer) rest
                    else
                        _run (tape, printer) loopCode |> loop 
                loop (tape, printer)
            | _ :: rest -> _run (tape, printer) rest // Comment and Input are not implemented, so just skip them
        _run (tape, printer) program
    
    let runWithNewTape printer program =
        run (Tape.create ()) printer program
        |> snd

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
        let source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
            ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

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
        then sprintf "%d != %d" left right |> Result.Error
        else Result.Ok None

    let isQuiet =
        Environment.GetEnvironmentVariable("QUIET")
        <> null

    let runtime =
        if isNull (Type.GetType("Mono.Runtime")) then ".NET Core" else "Mono"

open System.Diagnostics
open System.IO
    
[<EntryPoint>]
let main argv =
    match Util.verify () with
    | Error error ->
        printfn "%A" error
        exit 1
    | Ok _ -> ()

    match argv with
    | [| filename |] ->
        let source = File.ReadAllText filename
        let printer = if Util.isQuiet then Printer.createQuiet () else Printer.createStdout ()

        (Util.runtime, Process.GetCurrentProcess().Id)
        ||> sprintf "F#/%s\t%d"
        |> Util.notify

        let stopWatch = Stopwatch.StartNew()

        let pNew =
            Interpreter.parse source
            |> Interpreter.runWithNewTape printer

        stopWatch.Stop()

        let elapsed = stopWatch.Elapsed.TotalSeconds

        Util.notify "stop"
        printfn "time: %Os" elapsed

        if Printer.isQuiet pNew then
            Printer.checksum pNew |> printf "Output checksum: %d\n"
        0
    | _ ->
        printfn "usage: bf.fs <brainfuck.b>"
        1
