module Camera 
where

import Vec3 (Vec3, (.-), (.**), (.+))
import Ray hiding (origin)
import qualified Vec3 as V
import System.Random

data Camera = Camera 
  { lowerLeftCorner :: Vec3
  , horizontal :: Vec3
  , vertical :: Vec3
  , origin :: Vec3
  , lensRadius :: Double
  -- wat
  , u :: Vec3
  , v :: Vec3
  , w :: Vec3
  }

mkDefaultCamera :: Camera
mkDefaultCamera = Camera
 { lowerLeftCorner = (-2.0, -1.0, -1.0)
 , horizontal = (4.0, 0.0, 0.0)
 , vertical = (0.0, 2.0, 0.0)
 , origin = (0.0, 0.0, 0.0)
 , lensRadius = 0.0
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

  lowerLeftCorner = origin .- u .** halfWidth .** focusDist  .- v .** halfHeight .** focusDist .- w .** focusDist
  horizontal = u .** (halfWidth * 2)
  vertical = v .** (halfHeight * 2)
  in 
    Camera lowerLeftCorner horizontal vertical origin lensRadius u v w

randomInUnitDisk :: IO Vec3
randomInUnitDisk = do 
  x <- randomRIO (0.0, 1.1)
  y <- randomRIO (0.0, 1.1)
  pure $ ((x, y, 0) .- (1, 1, 0)) .** 2.0

getRay :: Camera -> Double -> Double -> IO Ray
getRay (Camera llc hor vert orig lr u v w) s t = do 
  rd <- (.** lr) <$> randomInUnitDisk
  let offset = u .** (V.x rd) .+ v .** (V.y rd) 
    in 
      pure $ Ray (orig .+ offset) (llc .+ (hor .** s) .+ (vert .** t) .- orig .- offset)