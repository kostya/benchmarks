{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Array.Base as ArrayBase
import qualified Data.Array.IO as IOUArray
import qualified Data.Array.MArray as MArray
import qualified Data.ByteString.Char8 as C
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.Maybe
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Exit
import System.IO (hFlush, stdout)
import System.Posix (getProcessID)
import Text.RawString.QQ

data Op = Inc !Int | Move !Int | Print | Loop ![Op] deriving Show
data Tape = Tape { tapeData :: IOUArray.IOUArray Int Int
                 , tapePos :: !Int
                 }
data Printer = Printer { sum1 :: Int
                       , sum2 :: Int
                       , quiet :: Bool
                       }

write :: Printer -> Int -> IO Printer
write p n = if quiet p
  then do let s1 = mod (sum1 p + n) 255
          let s2 = mod (s1 + sum2 p) 255
          return Printer {
            sum1=s1,
            sum2=s2,
            quiet=True
            }
  else do
  putStr [chr n]
  hFlush stdout
  return p

getChecksum :: Printer -> Int
getChecksum p = (sum2 p `shiftL` 8) .|. sum1 p

current :: Tape -> IO Int
current tape = ArrayBase.unsafeRead (tapeData tape) (tapePos tape)

inc :: Int -> Tape -> IO ()
inc delta tape = do
  prev <- current tape
  ArrayBase.unsafeWrite (tapeData tape) (tapePos tape) (prev + delta)

move :: Int -> Tape -> IO Tape
move m tape = do
    len <- ArrayBase.getNumElements curData
    newData <- if newPos < len
               then return curData
               else do
                 el <- MArray.getElems curData
                 MArray.newListArray (0, newPos)
                   (el ++ replicate (newPos - len + 1) 0)
    return $ Tape newData newPos
  where
    curData = tapeData tape
    newPos = tapePos tape + m

parse :: ([Char], [Op]) -> ([Char], [Op])
parse ([], acc) = ([], reverse acc)
parse (c:cs, acc) =
    case c of
        '+' -> parse (cs, Inc 1:acc)
        '-' -> parse (cs, Inc (-1):acc)
        '>' -> parse (cs, Move 1:acc)
        '<' -> parse (cs, Move (-1):acc)
        '.' -> parse (cs, Print:acc)
        '[' -> parse (newCs, Loop loop:acc)
                   where (newCs, loop) = parse (cs, [])
        ']' -> (cs, reverse acc)
        _   -> parse (cs, acc)

run :: [Op] -> Tape -> Printer -> IO (Tape, Printer)
run [] tape p = return (tape, p)
run (op:ops) tape p = do
    case op of
        Inc d -> do
            inc d tape
            run ops tape p
        Move m -> do
            newTape <- move m tape
            run ops newTape p
        Print -> do
            x <- current tape
            newP <- write p x
            run ops tape newP
        Loop loop ->
            let go (newTape, newP) = do
                x <- current newTape
                if x == 0 then run ops newTape newP
                else run loop newTape newP >>= go
            in go (tape, p)

notify :: String -> IO ()
notify msg = withSocketsDo $ do
  addr <- resolve
  catch (_notify addr) (\(_ :: IOException) -> return ())
  where
    writeMsg s = sendAll s $ C.pack msg
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just "localhost") (Just "9001")
    _notify addr = bracket (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      writeMsg sock

verify :: IO ()
verify = do
    let source = [r|++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
                   ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.|]
    let (_, ops) = parse (source, [])
    empty <- MArray.newListArray (0, 0) [0]
    (_, pLeft) <- run ops
      (Tape empty 0)
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
    notify $ "Haskell (MArray)\t" ++ show pid
    let (_, ops) = parse (source, [])
    empty <- MArray.newListArray (0, 0) [0]
    (_, newP) <- run ops (Tape empty 0) p
    notify "stop"

    when (quiet newP)
      $ do putStrLn $ "Output checksum: " ++ show (getChecksum newP)
