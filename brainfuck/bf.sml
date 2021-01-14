datatype opr = INC of int | MOVE of int | PRINT | LOOP of opr list
type tape = { data : int array, pos : int }
type printer = { sum1 : int, sum2: int, quiet: bool }

fun print (n, (p : printer)) =
    if #quiet p then
        let
            val newSum1 = (#sum1 p + n) mod 255
            val newSum2 = (newSum1 + #sum2 p) mod 255
        in
            { sum1 = newSum1, sum2 = newSum2, quiet = true }
        end
    else
        (
          TextIO.print (String.str (Char.chr n));
          TextIO.flushOut TextIO.stdOut;
          p
        )

fun getChecksum (p : printer) =
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
        val newPos = (#pos t) + m
        val len = Array.length (#data t)
        val newData =
            if newPos < len then #data t
            else
                let
                    val arr = Array.array (newPos + 1, 0)
                    val _ = Array.copy { src = #data t, dst = arr, di = 0 }
                in
                    arr
                end
    in
        { data = newData, pos = newPos }
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
                    val (newS, loopCode) = parse (rest, [])
                in
                    parse (newS, LOOP loopCode :: acc)
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
      | LOOP loopCode :: ops =>
        if current t = 0 then run ops t p
        else
            let
                val (newTape, newP) = run loopCode t p
            in
                run program newTape newP
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
        val (_, pLeft) = main source p
        val left = IntInf.toString (getChecksum pLeft)

        val seq = List.map Char.ord (explode("Hello World!\n"))
        val pRight = List.foldl print p seq
        val right = IntInf.toString (getChecksum pRight)
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
            val (_, newP) = main source p
        in
            notify("stop");
            if #quiet newP then
                let
                    val checksum = IntInf.toString (getChecksum newP)
                in
                    TextIO.print ("Output checksum: " ^ checksum ^ "\n")
                end
            else ()
        end
    end
