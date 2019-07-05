{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing -Wno-missing-signatures #-}
module Main where

import           Control.Monad
import           System.IO     (writeFile)
-- import           System.IO.Unsafe
import           System.Random (randomRIO)

import           Data.List     (intersperse)
import           Data.Monoid   (mconcat)

import           Camera        as C (getRay, mkDefaultCamera)
import           Hitable       (HitRecord (..), Hitable (..), Material (..))
import           Ray           (Ray (..))
import           Sphere        (Sphere (..))
import           Vec3          (Vec3, (.*), (.**), (.+), (.//))

import qualified Hitable       as H
import qualified Ray           as R
import qualified Vec3          as V

import           Utils         (toDouble)
import qualified Utils

width = 200
height = 100
antialiasingPassCount = 100
gamma = 2

main :: IO ()
main = do
  pic <- mkPicture
  writeFile "output.ppm" (mkPpmFile pic)

mkPicture :: IO [[Vec3]]
mkPicture =
  flapM js (\j -> flapM is (\i -> mkPixel i j))
  where
    is = [0..width-1]
    js =
    -- For some reason subtraction inside range declaration does not work and returns empty list =_=
      let
        j1 = height-1
        j2 = height-2
        in [j1, j2..0]

    flapM = flip mapM

    camera = C.mkDefaultCamera

    hitableList =
      [ Sphere (0, 0, -1) 0.5 (Lambertian (0.8, 0.3, 0.3))
      , Sphere (0, -100.5, -1) 100 (Lambertian (0.8, 0.8, 0.0))
      , Sphere (1, 0, -1) 0.5 (Metal (0.8, 0.6, 0.2))
      , Sphere (-1, 0, -1) 0.5 (Metal (0.8, 0.8, 0.8))
      ]
      -- ++ (unsafePerformIO $ replicateM 20 getRandomSphere)

    getRandomSphere :: IO Sphere
    getRandomSphere =
      let rand = randomRIO (-1.0, 1.0)
          randC = randomRIO (0.0, 1.0)
       in
        do
        x <- rand
        y <- rand
        rad <- randomRIO (0.01, 0.5)
        r <- randC
        g <- randC
        b <- randC
        matSelector <- rand
        mat <- if matSelector > 0
          then
            pure (Lambertian (r, g, b))
          else
            pure (Metal (r, g, b))
        pure (Sphere (x, y, -1) rad mat)

    mkPixel :: Int -> Int -> IO Vec3
    mkPixel i j = do
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
              pure ((toDouble i + r) / toDouble width)
            v <- do
              r <- randomRIO (0.0, 1.0)
              pure ((toDouble j + r) / toDouble height)
            color (C.getRay camera u v) hitableList 0

          formatColor :: Vec3 -> Vec3
          formatColor (r, g, b) = (norm r, norm g, norm b)
            where
              norm :: Double -> Double
              norm = (toDouble . truncate . (* 255.99) . (** (1.0 / (toDouble gamma))))

-- gets a color by tracing a ray against a list of hitable objects
color :: Hitable a => Ray -> [a] -> Int -> IO Vec3
color r hitables depth =
  case (hit hitables r 0.001 Utils.maxFloat) of

    Just rec@(HitRecord _ _ _ mat) -> do
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

mkPpmFile :: [[Vec3]] -> String
mkPpmFile pixels = header ++ body
  where
    header = mconcat ["P3\n", show width, " ", show height, "\n255\n"]

    join = mconcat . intersperse " "
    joinLine = mconcat . intersperse "\n"
    formatColor (r, g, b) = (join . map show) $ [ r, g, b ]

    body :: String
    body = joinLine . map (\x -> (joinLine . map formatColor) x) $ pixels
