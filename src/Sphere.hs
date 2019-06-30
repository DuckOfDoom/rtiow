module Sphere 
where

import Hitable (Hitable(..), HitRecord(..))

import Vec3 (Vec3, (.-), (.//))
import qualified Vec3 as V 
import qualified Ray as R 

data Sphere = Sphere
  { center :: Vec3
  , radius :: Double
  }

  -- hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
instance Hitable Sphere where
  hit s ray tMin tMax = let 
    oc = R.origin ray .- center s
    a = V.dot (R.direction ray) (R.direction ray)
    b = V.dot oc (R.direction ray)
    c = V.dot oc oc - radius s * radius s
    discriminant = b * b - a * c
    in
      if discriminant > 0 
        then
          let temp = (-b - sqrt (b * b - a * c)) / a in
          if temp < tMax && temp > tMin 
            then
              let hitPoint = (R.pointAt ray temp) in
              Just (HitRecord temp hitPoint ((hitPoint .- center s) .// radius s))
            else
              let temp' = (-b + sqrt (b * b - a * c) / a) in
              if temp' < tMax && temp' > tMin
                then
                  let hitPoint = (R.pointAt ray temp') in
                  Just (HitRecord temp' hitPoint ((hitPoint .- center s) .// radius s))
                else 
                  Nothing
        else 
          Nothing