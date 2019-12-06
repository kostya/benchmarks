import Control.Arrow (first)
import Data.Char (chr)
import Data.Function (fix)
import System.Environment (getArgs)
import System.IO (hFlush, hPutChar, stdout)

data Op = Inc | Dec | MoveL | MoveR | Print | Loop [Op]
    deriving Show

parse :: [Char] -> [Op]
parse = fst <$> go
  where
    go :: [Char] -> ([Op], [Char])
    go (c : cs) = case c of
        '+' -> first (Inc :) (go cs)
        '-' -> first (Dec :) (go cs)
        '<' -> first (MoveL :) (go cs)
        '>' -> first (MoveR :) (go cs)
        '.' -> first (Print :) (go cs)
        '[' -> first (Loop os :) (go cs')
            where (os, cs') = go cs
        ']' -> ([], cs)
        _ -> go cs
    go [] = ([], [])

data IntStream = !Int :- IntStream
    deriving Show

data Tape = Tape IntStream !Int IntStream
    deriving Show

blank :: Tape
blank = Tape (fix (0 :-)) 0 (fix (0 :-))

run :: [Op] -> Tape -> IO Tape
run (o : os) tape = case o of
    Inc -> let Tape ls v rs = tape
            in run os $ Tape ls (v + 1) rs
    Dec -> let Tape ls v rs = tape
            in run os $ Tape ls (v - 1) rs
    MoveL -> let Tape (l :- lt) v rs = tape
              in run os $ Tape lt l (v :- rs)
    MoveR -> let Tape ls v (r :- rt) = tape
              in run os $ Tape (v :- ls) r rt
    Print -> let Tape _ v _ = tape
              in hPutChar stdout (chr v) *> hFlush stdout *> run os tape
    Loop os' -> let Tape _ v _ = tape
                 in if v /= 0
                    then run (o : os) =<< run os' tape
                    else run os tape
run [] tape = pure tape

main :: IO ()
main = do
    [filename] <- getArgs
    source <- readFile filename
    _ <- run (parse source) blank
    pure ()
