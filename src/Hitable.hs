{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hitable 
where

import Ray
import Vec3

import Control.Monad.State
import Debug.Trace

data HitRecord = HitRecord  
  { t :: Double
  , p :: Vec3
  , normal :: Vec3
  }

class Hitable a where
  hit :: a -> Ray -> Double -> Double -> Maybe HitRecord

-- man this shit is ugly
instance (Hitable a) => Hitable [a] where
  hit [] _ _ _ = error "what the actual heck"
  hit [x] ray tMin tMax = hit x ray tMin tMax
  hit xs ray tMin tMax = execState (loop xs ray tMin tMax) Nothing 
    where
      loop :: (Hitable a) => [a] -> Ray -> Double -> Double -> State (Maybe HitRecord) ()
      loop [] _ _ _ = error "what the actual heck #2"
      loop [x] ray tMin tMax = do
        curr <- get
        case curr of 
          Nothing -> 
            -- put only Just values not to override the state
            maybe (pure ()) (put . Just) (hit x ray tMin tMax)
          Just (HitRecord newTMax _ _) -> 
            maybe (pure ()) (put . Just) (hit x ray tMin newTMax)

      loop (x:xs) ray tMin tMax = do
        curr <- get
        case curr of 
          Nothing ->
            loop [x] ray tMin tMax
          Just (HitRecord newTMax _ _) -> 
            loop [x] ray tMin newTMax 
        loop xs ray tMin tMax