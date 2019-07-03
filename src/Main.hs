{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}
module Main where

import           System.IO     (writeFile)
import           System.Random (randomRIO)
import           Control.Monad 

import           Data.List     (intersperse)
import           Data.Monoid   (mconcat)
import           Debug.Trace

import           Camera        as C (getRay, mkDefaultCamera)
import           Hitable       (HitRecord (..), Hitable (..))
import           Ray           (Ray (..))
import           Sphere        (Sphere (..))
import           Vec3          (Vec3, (.**), (.+), (.-), (.//))

import qualified Ray           as R
import qualified Vec3          as V

import qualified Utils
import Utils (toDouble)

import Debug.Trace

main :: IO ()
main = mkPpmFile >>= writeFile "output.ppm"

-- gets a color by tracing a ray against a list of hitable objects
color :: Hitable a => Ray -> [a] -> IO Vec3
color r hitables =
  case (hit hitables r 0.0 Utils.maxFloat) of

    Just (HitRecord _ p normal) -> do
      randInUnitSphere <- Utils.randomInUnitSphere
      target <- pure $ p .+ normal .+ randInUnitSphere
      fmap (.** 0.5) $ color (Ray p (target .- p)) hitables

    Nothing -> do
      let unitDirection = V.mkUnitVec3 (R.direction r)
          t = 0.5 * (V.y unitDirection + 1.0)
        in pure $ (1.0, 1.0, 1.0) .** (1.0 - t) .+ (0.5, 0.7, 1.0) .** t

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
      [ Sphere (0, 0, -1) 0.5 
      , Sphere (0, -100.5, -1) 100
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
            color (C.getRay camera u v) hitableList

          formatColor :: Vec3 -> String
          formatColor vec = 
            let normalizeColor =(** (1.0 / (toDouble gamma))) 
             in
              mconcat . intersperse " " . map (show .  (floor :: Double -> Integer) . (* 255.99) . normalizeColor) $ [ V.r vec,  V.g vec, V.b vec ]