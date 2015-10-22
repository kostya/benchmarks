{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (foldl')
import           GHC.Generics

newtype Structure = Structure { coordinates :: [Coordinate] }
    deriving (Generic)
instance J.FromJSON Structure
data Coordinate = Coordinate { x :: !Double
                             , y :: !Double
                             , z :: !Double }
    deriving (Generic)
instance J.FromJSON Coordinate

data Res = Res !Double !Double !Double !Int

main :: IO ()
main = do
    b <- BL.readFile "1.json"
    let struct = either error id (J.eitherDecode b)
    let res = foldl' (\(Res xsum ysum zsum count) (Coordinate{..}) ->
                          Res (xsum + x) (ysum + y) (zsum + z) (count + 1))
                     (Res 0 0 0 0)
                     (coordinates struct)
    let Res xsum ysum zsum count = res
    let c = fromIntegral count
    print (xsum / c)
    print (ysum / c)
    print (zsum / c)
