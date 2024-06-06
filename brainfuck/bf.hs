{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Text.RawString.QQ as QQ (r)
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.Maybe
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Exit
import System.IO
import System.Posix (getProcessID)

data Op = Inc !Int | MoveLeft | MoveRight | Print | Loop [Op]
data Tape = Tape { left :: [Int], current :: !Int, right :: [Int] }
data Printer = Printer { sum1 :: Int
                       , sum2 :: Int
                       , quiet :: Bool
                       }

write :: Printer -> Int -> IO Printer
write p n = if quiet p
  then do
  let newP = p {sum1 = mod (sum1 p + n) 255}
  return newP {sum2 = mod (sum1 newP + sum2 newP) 255}
  else do
  putStr [chr n]
  hFlush stdout
  return p

getChecksum :: Printer -> Int
getChecksum p = (sum2 p `shiftL` 8) .|. sum1 p

inc :: Int -> Tape -> Tape
inc delta (Tape l x r) = Tape l (x + delta) r

moveLeft, moveRight :: Tape -> Tape
moveLeft  (Tape ls n rs) = let (ls', n') = decomp ls in Tape ls' n' (n : rs)
moveRight (Tape ls n rs) = let (rs', n') = decomp rs in Tape (n : ls) n' rs'

decomp :: [Int] -> ([Int], Int)
decomp (x : xs) = (xs, x)
decomp [] = ([], 0)

parse :: ([Char], [Op]) -> ([Char], [Op])
parse ([], acc) = ([], reverse acc)
parse (c:cs, acc) =
    case c of
        '+'       -> parse (cs, Inc 1:acc)
        '-'       -> parse (cs, Inc (-1):acc)
        '>'       -> parse (cs, MoveRight:acc)
        '<'       -> parse (cs, MoveLeft:acc)
        '.'       -> parse (cs, Print:acc)
        '['       -> parse (newCs, Loop loop:acc)
                     where (newCs, loop) = parse (cs, [])
        ']'       -> (cs, reverse acc)
        _         -> parse (cs, acc)

run :: [Op] -> Tape -> Printer -> IO (Tape, Printer)
run [] tape p = return (tape, p)
run (op:ops) tape p = do
    case op of
        Inc d -> run ops (inc d tape) p
        MoveLeft -> run ops (moveLeft tape) p
        MoveRight -> run ops (moveRight tape) p
        Print -> do
          newP <- write p $ current tape
          run ops tape newP
        Loop loop -> do
            if current tape == 0
            then run ops tape p
            else do
                (newTape, newP) <- run loop tape p
                run (op:ops) newTape newP

notify :: String -> IO ()
notify msg = withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "localhost") (Just "9001")
  catch (_notify addr) (\(_ :: IOException) -> return ())
  where
    writeMsg s = sendAll s $ C.pack msg
    _notify addr = bracket (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      writeMsg sock

verify :: IO ()
verify = do
    let source = [QQ.r|++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
                       ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.|]
    let (_, ops) = parse (source, [])
    (_, pLeft) <- run ops
      (Tape [] 0 [])
      Printer {sum1=0, sum2=0, quiet=True}
    let left = getChecksum pLeft

    pRight <- foldM (\p c -> write p $ ord c)
                 (Printer {sum1=0, sum2=0, quiet=True})
                 "Hello World!\n"
    let right = getChecksum pRight
    when (left /= right)
      $ die $ show left ++ " != " ++ show right

main :: IO ()
main = do
    verify
    [filename] <- getArgs
    source <- readFile filename
    quiet_env <- lookupEnv "QUIET"
    let p = Printer {sum1=0, sum2=0, quiet=isJust quiet_env}

    pid <- getProcessID
    notify $ "Haskell (FP)\t" ++ show pid
    let (_, ops) = parse (source, [])
    (_, newP) <- run ops (Tape [] 0 []) p
    notify "stop"

    when (quiet newP)
      $ do putStrLn $ "Output checksum: " ++ show (getChecksum newP)
