module Main where

import Output (mkPpmFile)
import System.IO (writeFile)

main :: IO ()
main = writeFile "output.ppm" mkPpmFile
