module Main where

import qualified Data.Array.Base as ArrayBase
import qualified Data.Array.IO as IOUArray
import qualified Data.Array.MArray as MArray
import qualified Data.ByteString.Char8 as C
import Network.Simple.TCP
import Data.Char (chr)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data Op = Inc !Int | Move !Int | Print | Loop ![Op] deriving Show
data Tape = Tape { tapeData :: IOUArray.IOUArray Int Int
                 , tapePos :: !Int
                 }

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
                 MArray.newListArray (0, newPos) (el ++ [0 | i <- [len..newPos]])
    return $ Tape newData newPos
  where
    curData = tapeData tape
    newPos = (tapePos tape) + m

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

run :: [Op] -> Tape -> IO Tape
run [] tape = return tape
run (op:ops) tape = do
    case op of
        Inc d -> do
            inc d tape
            run ops tape
        Move m -> do
            newTape <- move m tape
            run ops newTape
        Print -> do
            x <- current tape
            putStr $ [chr x]
            hFlush stdout
            run ops tape
        Loop loop ->
            let go tape = do
                x <- current tape
                if x == 0 then run ops tape
                else run loop tape >>= go
            in go tape

main = do
    connect "localhost" "9001" $ \(socket, _) -> do
      send socket $ C.pack "Haskell MArray"
    [filename] <- getArgs
    source <- readFile filename
    let (_, ops) = parse (source, [])
    empty <- MArray.newListArray (0, 0) [0]
    run ops $ Tape empty 0
