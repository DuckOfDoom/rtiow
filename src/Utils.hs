module Utils 
 ( toDouble
 , maxFloat
 , randomInUnitSphere
 )
where

import System.Random
import Vec3 (Vec3)
import qualified Vec3 as V

toDouble :: Int -> Double
toDouble = fromIntegral

-- Need to use another method for picking random in sphere
randomInUnitSphere :: IO Vec3
randomInUnitSphere = do
  v <- randV
  if V.squaredLength v < 1 
    then pure v
    else randomInUnitSphere
  where
    rnd = randomRIO (-0.5, 0.5)
    randV = do 
        x <- rnd
        y <- rnd
        z <- rnd
        pure (x, y, z)

maxFloat :: Double
maxFloat = maxNonInfiniteFloat 0.5

---- We don't have a MAXFLOAT constant -_-
-- https://stackoverflow.com/questions/1780489/haskell-minimum-maximum-double-constant
maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e