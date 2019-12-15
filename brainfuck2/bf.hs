module Main where

import qualified Data.Array.Base as ArrayBase
import qualified Data.Array.Unboxed as UArray
import qualified Data.ByteString.Char8 as C
import Data.Char (chr)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Network.Simple.TCP

data Op = Inc Int | Move Int | Print | Loop [Op] deriving Show
data Tape = Tape { tapeData :: UArray.UArray Int Int
                 , tapePos :: Int
                 } deriving Show

current :: Tape -> Int
current tape = ArrayBase.unsafeAt (tapeData tape) (tapePos tape)

inc :: Int -> Tape -> Tape
inc delta tape =
    tape { tapeData = newData }
  where
    newData = ArrayBase.unsafeReplace (tapeData tape)
                                      [(tapePos tape, (current tape) + delta)]

move :: Int -> Tape -> Tape
move m tape =
    tape { tapeData = newData, tapePos = newPos }
  where
    curData = tapeData tape
    len = ArrayBase.numElements curData
    newPos = (tapePos tape) + m
    asc = ArrayBase.assocs curData
    newData = if newPos < len
              then curData
              else ArrayBase.unsafeArray (0, newPos)
                                         (asc ++ [(i, 0) | i <- [len..newPos]])

parse :: ([Char], [Op]) -> ([Char], [Op])
parse ([], acc) = ([], reverse acc)
parse (c:cs, acc) =
    case c of
        '+'       -> parse (cs, Inc 1:acc)
        '-'       -> parse (cs, Inc (-1):acc)
        '>'       -> parse (cs, Move 1:acc)
        '<'       -> parse (cs, Move (-1):acc)
        '.'       -> parse (cs, Print:acc)
        '['       -> parse (newCs, Loop loop:acc)
                     where (newCs, loop) = parse (cs, [])
        ']'       -> (cs, reverse acc)
        otherwise -> parse (cs, acc)

run :: [Op] -> Tape -> IO Tape
run [] tape = return tape
run (op:ops) tape = do
    case op of
        Inc d -> run ops $ inc d tape
        Move m -> run ops $ move m tape
        Print -> do
            putStr $ [chr $ current tape]
            hFlush stdout
            run ops tape
        Loop loop -> do
            if current tape == 0
            then run ops tape
            else do
                newTape <- run loop tape
                run (op:ops) newTape

main = do
    connect "localhost" "9001" $ \(socket, _) -> do
      send socket $ C.pack "Haskell"
    [filename] <- getArgs
    source <- readFile filename
    let (_, ops) = parse (source, [])
    run ops (Tape (ArrayBase.unsafeArray (0, 0) [(0, 0)]) 0)
