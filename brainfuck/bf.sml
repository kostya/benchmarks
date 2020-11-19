datatype opr = INC of int | MOVE of int | PRINT | LOOP of opr list
type tape = { data : int array, pos : int }
type printer = { sum1 : int, sum2: int, quiet: bool }

fun print (n, (p : printer)) =
    if #quiet p then
        let
            val new_sum1 = (#sum1 p + n) mod 255
            val new_sum2 = (new_sum1 + #sum2 p) mod 255
        in
            { sum1 = new_sum1, sum2 = new_sum2, quiet = true }
        end
    else
        (
          TextIO.print (String.str (Char.chr n));
          TextIO.flushOut TextIO.stdOut;
          p
        )

fun get_checksum (p : printer) =
    let
        val left = IntInf.<< (IntInf.fromInt (#sum2 p), Word.fromInt 8)
    in
        IntInf.orb (left, IntInf.fromInt (#sum1 p))
    end

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

fun run program (t : tape) (p : printer)=
    case program of
        [] => (t, p)
      | INC d :: ops => (inc d t; run ops t p)
      | MOVE m :: ops => run ops (move m t) p
      | PRINT :: ops => run ops t (print ((current t), p))
      | LOOP loop_code :: ops =>
        if current t = 0 then run ops t p
        else
            let
                val (new_tape, new_p) = run loop_code t p
            in
                run program new_tape new_p
            end

fun read_file filename =
    let
        val file = TextIO.openIn filename
        val contents = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in
        contents
    end

fun notify msg =
    let
        val localhost = valOf(NetHostDB.fromString "127.0.0.1")
        val addr = INetSock.toAddr(localhost, 9001)
        val sock = INetSock.TCP.socket()
        val _  = Socket.connect(sock, addr)
    in
        Socket.sendVec(sock, Word8VectorSlice.full(Byte.stringToBytes msg));
        Socket.close sock
    end
    handle OS.SysErr _ => ()

fun main source (p : printer) =
    let
        val (_, ops) = parse(source, [])
    in
        run ops { data = Array.fromList [0], pos = 0 } p
    end

fun verify () =
    let
        val source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>" ^
                     "---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
        val p = { sum1 = 0, sum2 = 0, quiet = true }
        val (_, p_left) = main source p
        val left = IntInf.toString (get_checksum p_left)

        val seq = List.map Char.ord (explode("Hello World!\n"))
        val p_right = List.foldl print p seq
        val right = IntInf.toString (get_checksum p_right)
    in
        if left <> right then (
            TextIO.output (TextIO.stdErr, left ^ " != " ^ right ^ "\n");
            OS.Process.exit OS.Process.failure
        ) else ()
    end

val () =
    let
        val args = CommandLine.arguments ()
        val source =
            case args of
                [filename] => read_file filename
              | _ => OS.Process.exit(OS.Process.failure)
        val pid = LargeWord.toInt
                      (Posix.Process.pidToWord
                           (Posix.ProcEnv.getpid ()))
        val quiet = OS.Process.getEnv "QUIET"
        val p = { sum1 = 0, sum2 = 0, quiet = isSome quiet }
    in
        verify();
        notify("MLton\t" ^ Int.toString pid);
        let
            val (_, new_p) = main source p
        in
            notify("stop");
            if #quiet new_p then
                let
                    val checksum = IntInf.toString (get_checksum new_p)
                in
                    TextIO.print ("Output checksum: " ^ checksum ^ "\n")
                end
            else ()
        end
    end
