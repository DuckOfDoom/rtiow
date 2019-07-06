{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Hitable
where

import           Ray
import           Vec3 (Vec3, (.+), (.-), (.**))

import           Control.Monad.State
import           System.Random

import qualified Utils
import qualified Vec3 as V
import qualified Ray as R

-- TODO: Remove IO
class Scattering a where
  scatter :: a -> Ray -> HitRecord -> IO (Maybe (Vec3, Ray))

data Material
  = Lambertian { albedo :: Vec3 } 
  | Metal { albedo :: Vec3 }
  | Dielectric { refIdx :: Double }
  deriving (Show)

instance Scattering Material where
  scatter (Lambertian a) _ (HitRecord _ p normal _) = do
    r <- Utils.randomInUnitSphere
    let 
      target = p .+ normal .+ r
      scattered = Ray p (target .- p)
      in 
      pure $ Just (a, scattered)

  scatter (Metal a) ray (HitRecord _ p normal _) = 
    let 
      reflected = V.reflect (V.mkUnitVec3 (R.direction ray)) normal
      scattered = Ray p reflected
      in 
      pure $ 
        if | V.dot (R.direction scattered) normal > 0 -> Just (a, scattered) 
           | otherwise -> Nothing

  scatter (Dielectric refIdx) ray (HitRecord _ p normal _) = 
    let 
      dir = R.direction ray
      schlick cosine refIdx' = 
        let
           r0 = (1 - refIdx') / (1 + refIdx')
           rsq = r0 * r0
           in rsq + (1 - rsq) * ((1 - cosine) ** 5)

      atten = (1.0, 1.0, 1.0)
      (outwardNormal, niOverNt, cosine) 
        | V.dot (R.direction ray) normal > 0 =
          ( normal .** (-1)
          , refIdx
          , refIdx * V.dot dir normal / V.length dir 
          )
        | otherwise = 
          ( normal
          , 1.0 / refIdx
          , - V.dot dir normal / V.length dir 
          )
      in
        case V.refract (R.direction ray) outwardNormal niOverNt of
          Just refracted -> do
              r <- randomRIO (0.0, 1.0)
              let reflProb = schlick cosine refIdx
                in pure $
                if| r < reflProb -> Just (atten, Ray p (V.reflect (R.direction ray) normal))
                  | otherwise -> Just (atten, Ray p refracted)
          Nothing -> pure $ Just (atten, Ray p (V.reflect (R.direction ray) normal))

data HitRecord = HitRecord
  { t        :: Double
  , p        :: Vec3
  , normal   :: Vec3
  , material :: Material
  }
  deriving (Show)

-- Type family?
class (Show a) => Hitable a where
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
          Just (HitRecord newTMax _ _ _) ->
            maybe (pure ()) (put . Just) (hit x ray tMin newTMax)

      loop (x:xs) ray tMin tMax = do
        curr <- get
        case curr of
          Nothing ->
            loop [x] ray tMin tMax
          Just (HitRecord newTMax _ _ _) ->
            loop [x] ray tMin newTMax
        loop xs ray tMin tMax