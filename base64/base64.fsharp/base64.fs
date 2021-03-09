open System
open System.Text
open System.Diagnostics

module Util =
    let runtimeId = 
        let runtime = if isNull (Type.GetType "Mono.Runtime") then ".NET Core" else "Mono"
        sprintf "F#/%O\t%d" runtime (Process.GetCurrentProcess().Id)

    let notify (msg: string) =
        try
            use s = new System.Net.Sockets.TcpClient("localhost", 9001)
            msg
                |> System.Text.Encoding.UTF8.GetBytes
                |> s.Client.Send
                |> ignore
        with _ -> ()

    let verify () =
        let check (s: string, b64s) = 
            let enc = Encoding.UTF8.GetBytes s |> Convert.ToBase64String
            let dec = Convert.FromBase64String b64s |> Encoding.UTF8.GetString
            match () with
            | _ when enc <> b64s -> Some <| sprintf "%A <> %A" enc b64s
            | _ when dec <> s -> Some <| sprintf "%A <> %A" dec s
            | _ -> None

        ["hello", "aGVsbG8="; "world", "d29ybGQ="]
            |> Seq.choose check
            |> fun seq -> if Seq.isEmpty seq then None else Some <| String.concat "\n" seq

[<EntryPoint>]
let main _ =
    Util.verify () |> Option.iter (fun errors -> eprintfn "%O" errors; exit 1)

    let STR_SIZE = 131072
    let TRIES = 8192

    let str1 = Array.replicate STR_SIZE 'a' |> Encoding.UTF8.GetBytes
    let str2 = Convert.ToBase64String str1
    let str3 = Convert.FromBase64String str2

    Util.notify Util.runtimeId

    let sw = Stopwatch.StartNew()

    let s_encoded = 
        seq { 1 .. TRIES } 
        |> Seq.map (fun _ -> Convert.ToBase64String str1)
        |> Seq.map String.length
        |> Seq.sum


    sw.Stop ()
    let t_encoded = sw.Elapsed.TotalSeconds

    sw.Restart ()

    let s_decoded = 
        seq { 1 .. TRIES } 
        |> Seq.map (fun _ -> Convert.FromBase64String str2)
        |> Seq.map Array.length
        |> Seq.sum

    sw.Stop()
    let t_decoded = sw.Elapsed.TotalSeconds

    Util.notify "Stop"

    printfn "encode %s... %s...: %O, %O" 
        (Encoding.UTF8.GetString(str1, 0, 4)) 
        (str2.Substring(0, 4)) 
        s_encoded 
        t_encoded

    printfn "decode %s... %s...: %O, %O" 
        (str2.Substring(0, 4)) 
        (Encoding.UTF8.GetString(str3, 0, 4)) 
        s_decoded
        t_decoded

    printfn "overall time: %Os" <| t_encoded + t_decoded
    0
