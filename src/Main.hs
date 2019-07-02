{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}
module Main where

import           System.IO     (writeFile)
import           System.Random (randomRIO)

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

main :: IO ()
main = mkPpmFile >>= writeFile "output.ppm"
-- main = writeFile "output.ppm" mkPpmFile'

hitSphere :: Ray -> Vec3 -> Double -> Double
hitSphere ray center radius
  | discriminant < 0 = -1.0
  | otherwise = (-b - sqrt discriminant) / 2.0 * a
  where
    dir = R.direction ray
    oc = R.origin ray .- center
    a = V.dot dir dir
    b = 2.0 * V.dot oc dir
    c = V.dot oc oc - radius * radius
    discriminant = b * b - 4 * a * c

-- gets a color by tracing a ray against a list of hitable objects
color :: Hitable a => Ray -> [a] -> Vec3
color r hitables =
  case (hit hitables r 0.0 Utils.maxFloat) of
    Just (HitRecord _ _ normal) ->
      (V.x normal + 1, V.y normal + 1, V.z normal + 1)  .** 0.5
    Nothing ->
        let unitDirection = V.mkUnitVec3 (R.direction r)
            t = 0.5 * (V.y unitDirection + 1.0)
        in (1.0, 1.0, 1.0) .** (1.0 - t) .+ (0.5, 0.7, 1.0) .** t

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

    camera = C.mkDefaultCamera

    hitableList =
      [ Sphere (0, -100.5, -1) 100
      -- , Sphere (0, 0, -1) 0.5
      ]

    mkLine :: Int -> Int -> IO String
    mkLine i j = do
      pColor <- getPixelColor i j
      pure ((mconcat . intersperse " " . map (show . floor)) (mkColorAsList pColor))
        where
          -- Runs a raytrace for each pixel multiple times then averages values
          getAverageColor :: Int -> Int -> IO Vec3
          getAverageColor i j = do
            list <- sequence $ replicate antialiasingPassCount (getPixelColor i j)
            pure $ (foldl1 (.+) list) .// (fromIntegral antialiasingPassCount)

          -- Runs a raytrace with random displacement to calculate antialiased pixel color
          getPixelColor :: Int -> Int -> IO Vec3
          getPixelColor i j = do
            u <- do
              r <- (randomRIO (0.0, 1.0))
              pure (fromIntegral i + r / fromIntegral width :: Double)
            v <- do
              r <- (randomRIO (0.0, 1.0))
              pure (fromIntegral j + r / fromIntegral height :: Double)
            pure $ color (C.getRay camera u v) hitableList

          mkColorAsList vec =
            [ 255.99 * V.r vec
            , 255.99 * V.g vec
            , 255.99 * V.b vec
            ]
