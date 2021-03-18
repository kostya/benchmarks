module JsonTest

open System.Text.Json
open System.Text.Json.Serialization

type Coordinate = { x: double; y: double; z: double }
type Root = { coordinates: List<Coordinate> }

let calc (text: string) =
    let jso = JsonSerializerOptions()
    jso.Converters.Add (JsonFSharpConverter())

    let root: Root = JsonSerializer.Deserialize (text, jso)
    let count = List.length root.coordinates |> double

    let folder (x, y, z) c = (c.x + x, c.y + y, c.z + z)

    let (x, y, z) = List.fold folder (0.0, 0.0, 0.0) root.coordinates

    { x = x / count; y = y / count; z = z / count }

module Util =
    open System.Diagnostics
    open System
    let notify (msg: string) =
        try
            use s = new Net.Sockets.TcpClient("localhost", 9001)
            msg
                |> System.Text.Encoding.UTF8.GetBytes
                |> s.Client.Send
                |> ignore
        with _ -> ()

    let runtimeId =
        sprintf "F#/.NET Core (System.Text.Json)\t%d"
            (Process.GetCurrentProcess().Id)

    let validate () =
        let expected = { x = 2.0; y = 0.5; z = 0.25 }
        [
            """{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}""";
            """{"coordinates":[{"y":0.5,"z":0.25,"x":2.0}]}"""]
        |> List.choose (fun s ->
            let actual = calc s
            if actual = expected then
                None
            else 
                Some <| sprintf "%A != %A" actual expected)
        |> function
        | [] -> None
        | errors -> Some <| String.concat "," errors


open System.IO
[<EntryPoint>]
let main argv =
    match Util.validate () with
    | Some error ->
        eprintf "%O" error
        exit 1
    | _ -> ()

    let text = File.ReadAllText "/tmp/1.json"
    Util.notify Util.runtimeId
    let result = calc text
    Util.notify "stop"

    printfn "Coordinate {X: %O, Y: %O, Z: %O}" result.x result.y result.z
    0
