module Camera 
where

import Vec3 (Vec3, (.-), (.**), (.+))
import Ray hiding (origin)
import qualified Vec3 as V

data Camera = Camera 
  { lowerLeftCorner :: Vec3
  , horizontal :: Vec3
  , vertical :: Vec3
  , origin :: Vec3
  }

mkDefaultCamera :: Camera
mkDefaultCamera = Camera
 { lowerLeftCorner = (-2.0, -1.0, -1.0)
 , horizontal = (4.0, 0.0, 0.0)
 , vertical = (0.0, 2.0, 0.0)
 , origin = (0.0, 0.0, 0.0)
 }

mkCamera :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Double -> Double -> Camera
mkCamera lookFrom lookAt vUp vFov aspect aperture focusDist = let
  lensRadius = aperture / 2.0
  theta = vFov * pi / 180.0
  halfHeight = tan (theta / 2.0)
  halfWidth = aspect * halfHeight
  origin = lookFrom
  w = V.mkUnitVec3 (lookFrom .- lookAt)
  u = V.mkUnitVec3 (V.cross vUp w)
  v = V.cross w u

  -- lowerLeftCorner = (-halfWidth, -halfHeight, -1.0)
  lowerLeftCorner = origin .- u .** halfWidth .** focusDist  .- v .** halfHeight .** focusDist .- w .** focusDist
  horizontal = u .** (halfWidth * 2)
  vertical = v .** (halfHeight * 2)
  in 
    Camera lowerLeftCorner horizontal vertical origin

-- getRay :: Camera -> Double -> Double -> Ray
-- getRay (Camera llc hor vert orig) u v = Ray orig (llc .+ (hor .** u) .+ (vert .** v) .- orig)

getRay :: Camera -> Double -> Double -> Ray
getRay (Camera llc hor vert orig) u v = Ray orig (llc .+ (hor .** u) .+ (vert .** v) .- orig)