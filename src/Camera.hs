{-# LANGUAGE RecordWildCards #-}
module Camera 
where

import Vec3
import Ray hiding (origin)

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

getRay :: Camera -> Double -> Double -> Ray
getRay Camera{..} u v = Ray origin (lowerLeftCorner .+ (horizontal .** u) .+ (vertical .** v) .- origin)