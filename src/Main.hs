{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}
module Main where

import System.IO (writeFile)

import Data.Monoid (mconcat)
import Data.List (intersperse)

import Vec3 (Vec3, (.-), (.**), (.+))
import Ray (Ray(..))

import qualified Vec3 as V 
import qualified Ray as R

main :: IO ()
main = writeFile "output.ppm" mkPpmFile

hitSphere :: Ray -> Vec3 -> Double -> Bool
hitSphere ray center radius = 
  let dir = R.direction ray
      oc = (R.origin ray) .- center
      a = V.dot dir dir
      b = 2.0 * V.dot oc dir 
      c = V.dot oc oc - radius * radius 
      discriminant = b * b - 4 * a * c
      in discriminant > 0

color :: Ray -> Vec3
color r 
  | hitSphere r (0, 0, -1) 0.5 = (1.0, 0, 0)
  | otherwise = let 
    unitDirection = V.mkUnitVec3 (R.direction r)
    t = 0.5 * (V.y unitDirection + 1.0)
    in (1.0, 1.0, 1.0) .** (1.0 - t) .+ (0.5, 0.7, 1.0) .** t

mkPpmFile :: String
mkPpmFile = 
  mconcat ["P3\n", show width, " ", show height, "\n255\n"] ++
  (unlines $
    flap js
      (\j ->
        unlines $ flap is (\i -> mkLine i j)) 
  )

  where 
    is = [0..width-1]
    js = 
    -- For some reason subtraction inside range declaration does not work and returns empty list =_=
      let 
        j1 = height-1
        j2 = height-2
        in [j1, j2..0]

    flap = flip map

    width = 200
    height = 100

    lowerLeftCorner = (-2.0, -1.0, -1.0)
    horizontal = (4.0, 0.0, 0.0)
    vertical = (0.0, 2.0, 0.0)
    origin = (0.0, 0.0, 0.0)

    mkLine :: Int -> Int -> String
    mkLine i j = 
      let 
        u = (fromIntegral i) / fromIntegral(width) :: Double
        v = (fromIntegral j) / fromIntegral(height) :: Double

        r = Ray origin (lowerLeftCorner .+ horizontal .** u .+ vertical .** v)
        col = color r

        ir = 255.99 * (V.r col)
        ig = 255.99 * (V.g col)
        ib = 255.99 * (V.b col)

        -- val :: Integer
        val = ((mconcat . intersperse " " . map (show . floor)) [ir, ig, ib])
        in
      val
        

