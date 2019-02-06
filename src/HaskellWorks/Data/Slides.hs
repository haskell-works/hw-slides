module HaskellWorks.Data.Slides where

import Diagrams.Backend.SVG
import Diagrams.Prelude

import qualified System.Directory as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

myDiagram :: Diagram B
myDiagram = square 1 # fc blue ||| circle 1 # fc blue

run :: IO ()
run = do
  IO.createDirectoryIfMissing True "output/hw-json"
  renderSVG "output/hw-json/output.svg" (mkWidth 250) myDiagram
