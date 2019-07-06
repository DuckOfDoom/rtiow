{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}

module Vec3
  ( Vec3
  , x
  , y
  , z
  , r
  , g
  , b
  , mkVec3
  , mkDefaultVec3
  , mkUnitVec3
  , (.+)
  , (.-)
  , (.*)
  , (./)
  , (.**)
  , (.//)
  , dot
  , cross
  , length
  , squaredLength
  , reflect
  , refract
  )
where

import Prelude hiding (length)

type Vec3 = (Double, Double, Double)

x (v, _, _) = v
y (_, v, _) = v
z (_, _, v) = v

r = x
g = y
b = z

mkVec3 :: Double -> Double -> Double -> Vec3
mkVec3 x y z = (x, y, z)

mkDefaultVec3 :: Vec3
mkDefaultVec3 = (0, 0, 0)

mkUnitVec3 :: Vec3 -> Vec3
mkUnitVec3 v = v .// (length v)

-- Not sure if these are needed, lets try removing them when everything works
infixl 6 .+, .-
infixl 7 .*, .**, ./, .//

(.+) :: Vec3 -> Vec3 -> Vec3
(x1, y1, z1) .+ (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

(.-) :: Vec3 -> Vec3 -> Vec3
(x1, y1, z1) .- (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

(.*) :: Vec3 -> Vec3 -> Vec3
(x1, y1, z1) .* (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)

(./) :: Vec3 -> Vec3 -> Vec3
(x1, y1, z1) ./ (x2, y2, z2) = (x1 / x2, y1 / y2, z1 / z2)

(.**) :: Vec3 -> Double -> Vec3
(x, y, z) .** t = (x * t, y * t, z * t)

(.//) :: Vec3 -> Double -> Vec3
(x, y, z) .// t = (x / t, y / t, z / t)

dot :: Vec3 -> Vec3 -> Double
(x1, y1, z1) `dot` (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
(x1, y1, z1) `cross` (x2, y2, z2) =
  ( y1 * z2 - z1 * y2
  , z1 * x2 - x1 * z2
  , x1 * y2 - y1 * x2
  )

length :: Vec3 -> Double
length v = sqrt(squaredLength v)

squaredLength :: Vec3 -> Double
squaredLength (x, y, z) = x*x + y*y + z*z

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v .- (n .** (2 * dot v n))

refract :: Vec3 -> Vec3 -> Double -> Maybe Vec3
refract v n niOverNt  = 
  let 
    uv = mkUnitVec3 v
    dt = dot uv n
    discriminant = 1.0 - niOverNt * niOverNt * (1 - dt * dt)
    in
    if discriminant > 0 then 
      Just $ (uv .- n .** dt) .** niOverNt .- n .** sqrt discriminant
    else
      Nothing