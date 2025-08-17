{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.List()
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString
import System.Exit
import System.Posix (getProcessID)

data Coordinate = Coordinate { x :: !Double
                             , y :: !Double
                             , z :: !Double }  deriving (Generic, Show, Eq)
instance FromJSON Coordinate

coords :: Value -> Parser [Coordinate]
coords = withObject "coords" (.: "coordinates")

data Res = Res !Double !Double !Double !Int

notify :: String -> IO ()
notify msg = withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "localhost") (Just "9001")
  catch (_notify addr) (\(_ :: IOException) -> return ())
  where
    writeMsg s = sendAll s $ C.pack msg
    _notify addr = bracket (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      writeMsg sock

calc :: BS.ByteString -> Coordinate
calc f = do
    let Res xsum ysum zsum count = foldl' op (Res 0 0 0 0)
          $ fromJust coordinates
    let c = fromIntegral count
    Coordinate (xsum / c) (ysum / c) (zsum / c)
 where
  coordinates = parseMaybe coords =<< decodeStrict' f
  op (Res xsum ysum zsum count) Coordinate{..} =
    Res (xsum + x) (ysum + y) (zsum + z) (count + 1)

verify :: (Coordinate, BS.ByteString) -> IO ()
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

  f <- BS.readFile "/tmp/1.json"
  pid <- getProcessID

  notify $ "Haskell\t" ++ show pid
  results <- return $! calc f
  notify "stop"

  print results
