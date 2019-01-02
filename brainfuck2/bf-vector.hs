import Data.Vector ((!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as U
import Data.Char (chr)
import System.Environment (getArgs)
import System.IO (hFlush, hPutChar, stdout)

data Op = Inc | Dec | MoveL | MoveR | Print | Bne Int | Beq Int  
    deriving Show

parse :: [Char] -> IO (V.Vector Op)
parse src = do
    ops <- M.unsafeNew len
    go [] src' ops 0
  where
    src' = filter (`elem` "+-<>.[]") src
    len = length src'

    go :: [Int] -> [Char] -> M.IOVector Op -> Int -> IO (V.Vector Op)
    go js (c : cs) ops i = case c of
        '+' -> do
            M.unsafeWrite ops i Inc
            go js cs ops (i + 1)
        '-' -> do
            M.unsafeWrite ops i Dec
            go js cs ops (i + 1)
        '<' -> do
            M.unsafeWrite ops i MoveL
            go js cs ops (i + 1)
        '>' -> do
            M.unsafeWrite ops i MoveR
            go js cs ops (i + 1)
        '.' -> do
            M.unsafeWrite ops i Print
            go js cs ops (i + 1)
        '[' -> do
            M.unsafeWrite ops i (Beq len)
            go (i : js) cs ops (i + 1)
        ']' -> case js of
            [] -> do
                M.unsafeWrite ops i (Bne 0)
                go [] cs ops (i + 1)
            j : jt -> do
                M.unsafeWrite ops i (Bne $ j + 1)
                M.unsafeWrite ops j (Beq $ i + 1)
                go jt cs ops (i + 1)
        _ -> do
            M.unsafeWrite ops i (Beq $ i + 1)
            go js cs ops (i + 1)
    go _ [] ops _ = V.unsafeFreeze ops

run :: V.Vector Op -> IO ()
run ops = do
    tape <- U.new 8
    go 0 tape 0
  where
    go :: Int -> U.IOVector Int -> Int -> IO ()
    go i tape j = j `seq` case ops !? i of
        Just Inc -> do
            v <- U.unsafeRead tape j
            U.unsafeWrite tape j (v + 1)
            go (i + 1) tape j
        Just Dec -> do
            v <- U.unsafeRead tape j
            U.unsafeWrite tape j (v - 1)
            go (i + 1) tape j
        Just MoveL -> go (i + 1) tape (j - 1)
        Just MoveR -> do
            let l = U.length tape
            if j + 1 >= U.length tape
            then do
                tape' <- U.grow tape l
                go (i + 1) tape' (j + 1)
            else go (i + 1) tape (j + 1)
        Just Print -> do
            v <- U.unsafeRead tape j
            hPutChar stdout $ chr v
            hFlush stdout
            go (i + 1) tape j
        Just (Bne k) -> do
            v <- U.unsafeRead tape j
            if v /= 0
            then go k tape j
            else go (i + 1) tape j
        Just (Beq k) -> do
            v <- U.unsafeRead tape j
            if v == 0
            then go k tape j
            else go (i + 1) tape j
        Nothing -> pure ()

main :: IO ()
main = do
    [filename] <- getArgs
    src <- readFile filename
    ops <- parse src
    run ops
