{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}
module Main where

import           Control.Monad
import           System.IO     (writeFile)
import           System.Random (randomRIO)

import           Data.List     (intersperse)
import           Data.Monoid   (mconcat)

import           Camera        as C (getRay, mkDefaultCamera)
import           Hitable       (HitRecord (..), Hitable (..), Material(..))
import           Ray           (Ray (..))
import           Sphere        (Sphere (..))
import           Vec3          (Vec3, (.*), (.**), (.+), (.-), (.//))

import qualified Ray           as R
import qualified Vec3          as V
import qualified Hitable as H

import           Utils         (toDouble)
import qualified Utils

main :: IO ()
main = mkPpmFile >>= writeFile "output.ppm"

-- gets a color by tracing a ray against a list of hitable objects
color :: Hitable a => Ray -> [a] -> Int -> IO Vec3
color r hitables depth =
  case (hit hitables r 0.0 Utils.maxFloat) of

    Just rec@(HitRecord _ p normal mat) -> do
      if depth < 50 then do
        scatterResult <- (H.scatter mat r rec)
        case scatterResult of
          Just (attenuation, scattered) -> 
            fmap (.* attenuation) $ color scattered hitables (depth - 1) 
          Nothing -> pure (0.0, 0.0, 0.0)
        else
          pure (0.0, 0.0, 0.0)

    Nothing -> do
      let unitDirection = V.mkUnitVec3 (R.direction r)
          t = 0.5 * (V.y unitDirection + 1.0)
        in pure $ (1.0, 1.0, 1.0) .** (1.0 - t) .+ (0.5, 0.7, 1.0) .** t

  where

mkPpmFile :: IO String
mkPpmFile = do
  picture <- unlines <$> flapM js
    (\j -> unlines <$> flapM is
      (\i -> mkLine i j)
    )
  pure $ mconcat ["P3\n", show width, " ", show height, "\n255\n"] ++ picture

  where
    is = [0..width-1]
    js =
    -- For some reason subtraction inside range declaration does not work and returns empty list =_=
      let
        j1 = height-1
        j2 = height-2
        in [j1, j2..0]

    flapM = flip mapM

    width = 200
    height = 100
    antialiasingPassCount = 100
    gamma = 2

    camera = C.mkDefaultCamera

    hitableList =
      [ Sphere (0, 0, -1) 0.5 (Lambertian (0.8, 0.3, 0.3))
      , Sphere (0, -100.5, -1) 100 (Lambertian (0.8, 0.8, 0.0))
      , Sphere (1, 0, -1) 0.5 (Metal (0.8, 0.6, 0.2))
      , Sphere (-1, 0, -1) 0.5 (Metal (0.8, 0.8, 0.8))
      ]

    mkLine :: Int -> Int -> IO String
    mkLine i j = do
      pColor <- getAverageColor i j
      pure $ formatColor pColor
        where
          getAverageColor :: Int -> Int -> IO Vec3
          getAverageColor i j = do
            list <- replicateM antialiasingPassCount (getPixelColor i j)
            pure $ foldl1 (.+) list .// fromIntegral antialiasingPassCount

          -- Runs a raytrace with random displacement to calculate antialiased pixel color
          getPixelColor :: Int -> Int -> IO Vec3
          getPixelColor i j = do
            u <- do
              r <- randomRIO (0.0, 1.0)
              pure ((toDouble i + r) / toDouble width )
            v <- do
              r <- randomRIO (0.0, 1.0)
              pure ((toDouble j + r) / toDouble height)
            color (C.getRay camera u v) hitableList 0

          formatColor :: Vec3 -> String
          formatColor vec =
            let normalizeColor = ((floor :: Double -> Integer) . (* 255.99) . (** (1.0 / (toDouble gamma))))
             in
              mconcat . intersperse " " . map (show .  normalizeColor) $ [ V.r vec,  V.g vec, V.b vec ]
