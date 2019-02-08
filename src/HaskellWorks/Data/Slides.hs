{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module HaskellWorks.Data.Slides where

import Diagrams.Backend.SVG
import Diagrams.Prelude

import qualified System.Directory as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

myDiagram :: Diagram B
myDiagram = square 1 # fc blue ||| circle 1 # fc blue ||| example

illustrateBézier c1 c2 x2
    =  endpt
    <> endpt  # translate x2
    <> ctrlpt # translate c1
    <> ctrlpt # translate c2
    <> l1
    <> l2
    <> fromSegments [bézier3 c1 c2 x2]
  where dashed  = dashingN [0.03, 0.03] 0
        endpt   = circle 0.05 # fc red  # lw none
        ctrlpt  = circle 0.05 # fc blue # lw none
        l1      = fromOffsets [c1] # dashed
        l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed

x2        = r2 (3, -1) :: V2 Double     -- endpoint
[c1, c2]  = map r2 [(1, 2), (3, 0)]     -- control points

example = illustrateBézier c1 c2 x2

run :: IO ()
run = do
  IO.createDirectoryIfMissing True "output/hw-json"
  renderSVG "output/hw-json/output.svg" (mkWidth 250) myDiagram
