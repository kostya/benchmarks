{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Aeson             as J
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Char8 as C
import Control.Exception
import Control.Monad
import Data.JsonStream.Parser
import Data.List (foldl')
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString
import System.Exit
import System.Posix (getProcessID)

data Coordinate = Coordinate { x :: !Double
                             , y :: !Double
                             , z :: !Double }  deriving (Generic, Show, Eq)
instance J.FromJSON Coordinate

data Res = Res !Double !Double !Double !Int

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

calc :: BL.ByteString -> Coordinate
calc f = do
    let Res xsum ysum zsum count = foldl' op (Res 0 0 0 0)
                                 $ parseLazyByteString parser_coordinates f
    let c = fromIntegral count
    Coordinate (xsum / c) (ysum / c) (zsum / c)
 where
  parser_coordinates = "coordinates" .: arrayOf coord
  coord = Coordinate <$> ("x" .: real) <*> ("y" .: real) <*> ("z" .: real)
  op (Res xsum ysum zsum count) Coordinate{..} =
    Res (xsum + x) (ysum + y) (zsum + z) (count + 1)

verify :: (Coordinate, BL.ByteString) -> IO ()
verify (right, v) = do
  let left = calc v
  when (left /= right)
    $ die $ show left ++ " != " ++ show right

main :: IO ()
main = do
  let right = Coordinate 2.0 0.5 0.25
  let verification_pairs = map (right,)
        ["{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
         "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"]
  mapM_ verify verification_pairs

  f <- BL.readFile "/tmp/1.json"
  pid <- getProcessID

  notify $ "Haskell\t" ++ show pid
  results <- return $! calc f
  notify "stop"

  print results
