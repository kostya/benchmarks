{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Data.Aeson             as J
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Char8 as C
import           Data.JsonStream.Parser
import           Data.List              (foldl')
import           GHC.Generics
import           Network.Simple.TCP
import System.Posix (getProcessID)
import System.Exit

data Coordinate = Coordinate { x :: !Double
                             , y :: !Double
                             , z :: !Double }  deriving (Generic, Show, Eq)
instance J.FromJSON Coordinate

data Res = Res !Double !Double !Double !Int

notify :: String -> IO ()
notify msg = do
    connect "localhost" "9001" $ \(socket, _) -> do
      send socket $ C.pack msg

calc :: BL.ByteString -> Coordinate
calc f = do
    let Res xsum ysum zsum count = foldl' op (Res 0 0 0 0)
                                 $ parseLazyByteString parser_coordinates f
    let c = fromIntegral count
    Coordinate (xsum / c) (ysum / c) (zsum / c)
 where
  parser_coordinates = "coordinates" .: arrayOf coord
  coord = Coordinate <$> ("x" .: real) <*> ("y" .: real) <*> ("z" .: real)
  op (Res xsum ysum zsum count) (Coordinate{..}) =
    Res (xsum + x) (ysum + y) (zsum + z) (count + 1)

verify :: (Coordinate, BL.ByteString) -> IO ()
verify (right, v) = do
  let left = calc $ v
  if left /= right
    then die $ show(left) ++ " != " ++ show(right)
  else return ()

main :: IO ()
main = do
  let right = Coordinate 1.1 2.2 3.3
  let verification_pairs = map (\x -> (right, x))
        ["{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}",
         "{\"coordinates\":[{\"y\":2.2,\"x\":1.1,\"z\":3.3}]}"]
  _ <- sequence (map verify verification_pairs)

  f <- BL.readFile "/tmp/1.json"
  pid <- getProcessID
  notify $ "Haskell\t" ++ show pid

  print $ calc $ f

  notify "stop"
