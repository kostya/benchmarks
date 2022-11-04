module Main

import Control.Monad.State as St
import Data.String as Str
import Data.Bits as B
import Network.Socket as Skt
import System as Sys
import System.File as F

data Op = Inc Int | Left | Right | Print | Loop (List Op)
data Tape = MkTape (List Int) Int (List Int)

current : Tape -> Int
current (MkTape _ n _) = n

inc : Int -> Tape -> Tape
inc d (MkTape l n r) = MkTape l (n + d) r

decomp : List Int -> (List Int, Int)
decomp (x :: xs) = (xs, x)
decomp [] = ([], 0)

left, right: Tape -> Tape
left  (MkTape ls n rs) = let (ls', n') = decomp ls in MkTape ls' n' (n :: rs)
right (MkTape ls n rs) = let (rs', n') = decomp rs in MkTape (n :: ls) n' rs'

parse : List Char -> List Op
parse cs = let (_, ops) = loop (cs, []) in ops
  where
    loop : (List Char, List Op) -> (List Char, List Op)
    loop ([], acc) = ([], reverse acc)
    loop (c :: cs, acc) = case c of
      '+' => loop (cs, Inc 1 :: acc)
      '-' => loop (cs, Inc (-1) :: acc)
      '>' => loop (cs, Right :: acc)
      '<' => loop (cs, Left :: acc)
      '.' => loop (cs, Print :: acc)
      '[' => let (cs', body) = loop (cs, []) in loop (cs', Loop body :: acc)
      ']' => (cs, reverse acc)
      _   => loop (cs, acc)

interface Monad m => Ctx m where
  write : Int -> m ()

checkSum : (Int, Int) -> Int
checkSum (s₁, s₂) = s₂ `shiftL` 8 .|. s₁

accCheckSum : Int -> (Int, Int) -> (Int, Int)
accCheckSum n (s₁, s₂) = let s₁' = (s₁ + n) `mod` 255
                             s₂' = (s₁' + s₂) `mod` 255
                         in (s₁', s₂')

-- For benchmark
Loud = IO
Ctx Loud where
  write n = do
    putChar (chr n)
    fflush stdout

-- For checksum-ing
Quiet = State (Int, Int)
Ctx Quiet where
  write n = modify (accCheckSum n)

partial
run : Ctx m => List Op -> Tape -> m Tape
run [] tape = pure tape
run (op :: ops) tape = do
  case op of
    Inc d => run ops (inc d tape)
    Left => run ops (left tape)
    Right => run ops (right tape)
    Print => do
        write (current tape)
        run ops tape
    Loop body => case current tape of
        0 => run ops tape
        _ => do
          tape' <- run body tape
          run (op :: ops) tape'

runFresh : Ctx m => List Op -> m Tape
runFresh ops = run ops (MkTape [] 0 [])

runQuiet : List Op -> (Int, Int)
runQuiet = execState (0, 0) . runFresh

verified =
  let src = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
      ops = parse (unpack src)
      csActual = runQuiet ops
      csExpected = foldl (\cs, c => accCheckSum (ord c) cs) (0, 0) (unpack "Hello World!\n")
  in csActual == csExpected

partial
notify : String -> IO ()
notify msg = do
  Right skt <- socket AF_INET Stream 0
  _ <- connect skt (Hostname "localhost") 9001
  _ <- send skt msg
  close skt

partial
main : IO ()
main = do
  let True = verified
  [_, fn] <- getArgs
  Right src <- readFile fn
  quiet <- getEnv "QUIET"
  pid <- getPID
  notify $ "Idris\t" ++ show pid
  let ops = parse (unpack src)
  case quiet of
      Just _  => let cs = runQuiet ops
                 in do
                   notify "stop"
                   putStrLn $ "Output checksum: " ++ show (checkSum cs)
      Nothing => do
                   _ <- runFresh ops
                   notify "stop"
