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

data Coordinate = Coordinate { x :: !Double
                             , y :: !Double
                             , z :: !Double }  deriving (Generic)
instance J.FromJSON Coordinate

data Res = Res !Double !Double !Double !Int

notify :: String -> IO ()
notify msg = do
    connect "localhost" "9001" $ \(socket, _) -> do
      send socket $ C.pack msg

main :: IO ()
main = do
    f <- BL.readFile "/tmp/1.json"
    pid <- getProcessID
    notify $ "Haskell\t" ++ show pid
    let Res xsum ysum zsum count = foldl' op  (Res 0 0 0 0)
                                 $ parseLazyByteString parser_coordinates f
    let c = fromIntegral count
    print (xsum / c)
    print (ysum / c)
    print (zsum / c)

    notify "stop"
 where
  parser_coordinates = "coordinates" .: arrayOf coord
  coord = Coordinate <$> ("x" .: real) <*> ("y" .: real) <*> ("z" .: real)
  op (Res xsum ysum zsum count) (Coordinate{..}) = Res (xsum + x) (ysum + y) (zsum + z) (count + 1)
