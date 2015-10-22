{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (foldl')
import           GHC.Generics

data Structure = Structure { coordinates :: ![!Coordinate] }
    deriving (Generic)
instance J.FromJSON Structure
data Coordinate = Coordinate { x :: !Double
                             , y :: !Double
                             , z :: !Double }
    deriving (Generic)
instance J.FromJSON Coordinate

main :: IO ()
main = do
    b <- BL.readFile "1.json"
    let struct = either error id (J.eitherDecode b)
    let res :: (Double, Double, Double, Int)
        res = foldl' (\(xsum, ysum, zsum, count) c ->
                          (xsum + x c, ysum + y c, zsum + z c, count + 1))
                     (0, 0, 0, 0)
                     (coordinates struct)
    let (xsum, ysum, zsum, count) = res
    let c = fromIntegral count
    print (xsum / c)
    print (ysum / c)
    print (zsum / c)
