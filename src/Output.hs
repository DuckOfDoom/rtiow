module Output 
 ( mkPpmFile ) 
where

import Data.Monoid (mconcat, (<>))
import Data.List (intersperse)

mkPpmFile :: String
mkPpmFile = 
  mconcat ["P3\n", show width, " ", show height, "\n255\n"] ++
  (unlines $
    flap (reverse [0..height-2]) 
      (\j ->
        unlines $ flap [0..width-1] (\i -> mkLine i j)) 
  )

  where 
    flap = flip map
    unlines = mconcat . intersperse "\n"

    width = 200
    height = 100

    mkLine :: Int -> Int -> String
    mkLine i j = 
      let 
          r = (fromIntegral i) / (fromIntegral width) :: Double
          g = (fromIntegral j) / (fromIntegral height) :: Double
          b = 0.2 
          ir = 255.99 * r  
          ig = 255.99 * g
          ib = 255.99 * b
          val = ((mconcat . intersperse " " . map (show . floor)) [ir, ig, ib])
          in
      val
        

