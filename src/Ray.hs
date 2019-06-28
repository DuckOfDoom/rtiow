{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Ray

where

import           Vec3 (Vec3, (.**), (.+))

data Ray = Ray
 { a :: Vec3
 , b :: Vec3
 }
 deriving (Show)

direction = b
origin = a

pointAt :: Ray -> Double -> Vec3
pointAt r t = (a r) .+ ((b r) .** t)
