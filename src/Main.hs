{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing -Wno-missing-signatures #-}
module Main where

import           Control.Monad
import           System.IO        (writeFile)
import           System.IO.Unsafe

import           System.Random    (randomRIO)

import           Data.List        (intersperse)
import           Data.Monoid      (mconcat)

import           Camera           as C (getRay, mkCamera)
import           Hitable          (HitRecord (..), Hitable (..), Material (..))
import           Ray              (Ray (..))
import           Sphere           (Sphere (..))
import           Vec3             (Vec3, (.*), (.**), (.+), (.-), (.//))

import qualified Hitable          as H
import qualified Ray              as R
import qualified Vec3             as V

import           Utils            (toDouble)
import qualified Utils

import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as BS
import           Graphics.Gloss
import           Data.List.Split (chunksOf)
import           Debug.Trace

-- import qualified Streamly.Prelude as S
import Conduit 

import Control.DeepSeq (force)

import Codec.Picture 
import Codec.Picture.Png

width = 200
height = 100
antialiasingPassCount = 100
gamma = 2

main :: IO ()
main = do
  pic <- mkPicture
  saveAsPng "output.png" pic
  -- showPicture pic

showPicture :: [Vec3] -> IO ()
showPicture pic = let
  convertPixel :: Vec3 -> ByteString
  convertPixel (r, g, b) = BS.pack (map (fromIntegral . truncate) [ r, g, b, 255 ])
  bitmapData :: ByteString
  bitmapData = BS.concat . map convertPixel $ pic
  picture = bitmapOfByteString width height (BitmapFormat TopToBottom PxRGBA) bitmapData True
  in display (InWindow "Balls" (width, height) (960 - 100, 600 - 50)) white picture

saveAsPng :: FilePath -> [Vec3] -> IO ()
saveAsPng path pic = let 
  vec3ToPixel (r, g, b) = PixelRGB8 (truncate r) (truncate g) (truncate b)
  pixelTable = chunksOf width pic
  img = generateImage (\x y -> vec3ToPixel $ pixelTable !! y !! x) width height
  in
  writePng path img
  
saveAsPpm :: FilePath -> [Vec3] -> IO ()
saveAsPpm path pixels = writeFile path mkPpmFile
  where
    mkPpmFile = header ++ body
    header = mconcat ["P3\n", show width, " ", show height, "\n255\n"]

    join = mconcat . intersperse " "
    joinLine = mconcat . intersperse "\n"
    formatColor (r, g, b) = join . map show $ [ r, g, b ]

    insertNewlines :: [String] -> [String]
    insertNewlines = loop 0
      where 
        loop _ [] = []
        loop i (x:xs)
         | i >= (width - 1) = x : "\n" : loop 0 xs
         | otherwise = x : " " : loop (i + 1) xs
        loop _ x = x
 
    body :: String
    body = (mconcat . insertNewlines . map formatColor) pixels
  
mkPicture :: IO [Vec3]
mkPicture =
  mapM mkPixel mkCoords
  -- forM js (\j -> forM is (\i -> mkPixel i j))
  -- S.toList $ S.mapM mkPixel $ S.fromList mkCoords

  -- runConduit $ yieldMany mkCoords .| mapMC mkPixel .| sinkList

  where
    is = [0..width-1]
    js =
    -- For some reason subtraction inside range declaration does not work and returns empty list =_=
      let
        j1 = height-1
        j2 = height-2
        in [j1, j2..0]

    mkCoords :: [(Int, Int)]
    mkCoords = foldl1 (++) $ map (\i -> map (\j -> (j, i)) js) is

    -- Camera creation
    lookFrom = (-2.0, 2.0, 1.0)
    lookAt = (0.0, 0.0, -1.0)
    vUp = (0.0, 1.0, 0.0)
    distToFocus = V.length (lookFrom .- lookAt)
    aperture = 2.0
    ratio = fromIntegral width / fromIntegral height

    camera = C.mkCamera lookFrom lookAt vUp 20 ratio aperture distToFocus

    hitableList =
      [ Sphere (0, 0, -1) 0.5 (Lambertian (0.1, 0.2, 0.5))
      , Sphere (0, -100.5, -1) 100 (Lambertian (0.8, 0.8, 0.0))
      , Sphere (1, 0, -1) 0.5 (Metal (0.8, 0.6, 0.2))
      , Sphere (-1, 0, -1) 0.5 (Dielectric 1.5)
      , Sphere (-1, 0, -1) (-0.45) (Dielectric 1.5)
      ] -- ++ randomSpheres 30

    randomSpheres :: Int -> [Sphere]
    randomSpheres count =
        unsafePerformIO $ replicateM count $ do
        x <- randomRIO (-1.0, 1.0)
        y <- randomRIO (-1.0, 1.0)
        rad <- randomRIO (0.01, 0.3)
        r <- randomRIO (0.0, 1.0)
        g <- randomRIO (0.0, 1.0)
        b <- randomRIO (0.0, 1.0)
        matSelector <- randomRIO (0, 1) :: IO Int
        mat <- if matSelector == 0
          then
            pure (Lambertian (r, g, b))
          else
            pure (Metal (r, g, b))
        pure (Sphere (x, y, -1) rad mat)

    mkPixel :: (Int, Int) -> IO Vec3
    mkPixel (i, j) = do 
      pColor <- if antialiasingPassCount > 1
        then
          force getAverageColor i j
        else
          force getPixelColor i j

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
            ray <- C.getRay camera u v
            computeColor ray hitableList 0 -- depth?

          formatColor :: Vec3 -> Vec3
          formatColor (r, g, b) = (norm r, norm g, norm b)
            where
              norm :: Double -> Double
              norm = toDouble . truncate . (* 255.99) . (** (1.0 / toDouble gamma))

-- gets a color by tracing a ray against a list of hitable objects
computeColor :: Hitable a => Ray -> [a] -> Int -> IO Vec3
computeColor r hitables depth =
  case hit hitables r 0.001 Utils.maxFloat of

    Just rec@(HitRecord _ _ _ mat) ->
      if depth < 50 then do
        scatterResult <- H.scatter mat r rec
        case scatterResult of
          Just (attenuation, scattered) ->
            (.* attenuation) <$> computeColor scattered hitables (depth - 1)
          Nothing -> pure (0.0, 0.0, 0.0)
        else
          pure (0.0, 0.0, 0.0)

    Nothing ->
      let unitDirection = V.mkUnitVec3 (R.direction r)
          t = 0.5 * (V.y unitDirection + 1.0)
        in pure $ (1.0, 1.0, 1.0) .** (1.0 - t) .+ (0.5, 0.7, 1.0) .** t
