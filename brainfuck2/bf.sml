type tape = { data : int array, pos : int }
datatype opr = INC of int | MOVE of int | PRINT | LOOP of opr list

fun current (t : tape) =
  Array.sub (#data t, #pos t)

fun inc delta (t : tape) =
  Array.update (#data t, #pos t, (current t) + delta)

fun move m (t : tape) =
  let
    val new_pos = (#pos t) + m
    val len = Array.length (#data t)
    val new_data =
      if new_pos < len then #data t
      else
        let
          val arr = Array.array (new_pos + 1, 0)
          val _ = Array.copy { src = #data t, dst = arr, di = 0 }
        in
          arr
        end
  in
    { data = new_data, pos = new_pos }
  end

fun parse (s, acc) =
  if s = "" then ("", List.rev acc)
  else
    let
      val c = String.sub (s, 0)
      val rest = String.extract (s, 1, NONE)
    in
      case c of
        #"+" => parse (rest, INC 1 :: acc)
      | #"-" => parse (rest, INC ~1 :: acc)
      | #">" => parse (rest, MOVE 1 :: acc)
      | #"<" => parse (rest, MOVE ~1 :: acc)
      | #"." => parse (rest, PRINT :: acc)
      | #"[" =>
          let
            val (new_s, loop_code) = parse (rest, [])
          in
            parse (new_s, LOOP loop_code :: acc)
          end
      | #"]" => (rest, List.rev acc)
      | _ => parse (rest, acc)
    end

fun run program (t : tape) =
  case program of
    [] => t
  | INC d :: ops => (inc d t; run ops t)
  | MOVE m :: ops => run ops (move m t)
  | PRINT :: ops =>
      (
        TextIO.print (String.str (Char.chr (current t)));
        TextIO.flushOut TextIO.stdOut;
        run ops t
      )
  | LOOP loop_code :: ops =>
      if current t = 0 then run ops t
      else
        let
          val new_tape = run loop_code t
        in
          run program new_tape
        end

fun read_file filename =
  let
    val file = TextIO.openIn filename
    val contents = TextIO.inputAll file
    val _ = TextIO.closeIn file
  in
    contents
  end

val _ =
let
  val localhost = valOf(NetHostDB.fromString "127.0.0.1")
  val addr = INetSock.toAddr(localhost, 9001)
  val sock = INetSock.TCP.socket()
  val _  = Socket.connect(sock, addr)
in
  Socket.sendVec(sock, Word8VectorSlice.full(Byte.stringToBytes "MLton"));
  Socket.close sock
end
handle OS.SysErr _ => ()

val args = CommandLine.arguments ()
val source =
  case args of
    [filename] => read_file filename
  | _ => OS.Process.exit(OS.Process.failure)
val (_, ops) = parse(source, [])
val _ = run ops { data = Array.fromList [0], pos = 0 }
