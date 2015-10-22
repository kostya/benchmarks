{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Data.Aeson             as J
import qualified Data.ByteString.Lazy   as BL
import           Data.JsonStream.Parser
import           Data.List              (foldl')
import           GHC.Generics

data Coordinate = Coordinate { x :: !Double
                             , y :: !Double
                             , z :: !Double }  deriving (Generic)
instance J.FromJSON Coordinate

data Res = Res !Double !Double !Double !Int

main :: IO ()
main = do
    f <- BL.readFile "1.json"
    let Res xsum ysum zsum count = foldl' op  (Res 0 0 0 0)
                                 $ parseLazyByteString parser_coordinates f
    let c = fromIntegral count
    print (xsum / c)
    print (ysum / c)
    print (zsum / c)
 where
  parser_coordinates = "coordinates" .: arrayOf coord
  coord = Coordinate <$> ("x" .: real) <*> ("y" .: real) <*> ("z" .: real)
  op (Res xsum ysum zsum count) (Coordinate{..}) = Res (xsum + x) (ysum + y) (zsum + z) (count + 1)
