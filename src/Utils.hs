module Utils 
 ( maxFloat )
where

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