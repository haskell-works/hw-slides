{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module HaskellWorks.Data.Slides where

import Data.Typeable
import Diagrams.Backend.SVG
import Diagrams.Prelude

import qualified Data.Char        as C
import qualified System.Directory as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Reundant do"        :: String) #-}

selectTransition :: Char -> Diagram B
selectTransition c | C.isSpace c  = transition1 [c]
selectTransition c | c == '{'     = transition1 [c]
selectTransition c | c == '['     = transition1 [c]
selectTransition c | c == ']'     = transition1 [c]
selectTransition c | c == '}'     = transition1 [c]
selectTransition c | c == ','     = transition1 [c]
selectTransition c | c == ':'     = transition1 [c]
selectTransition c | c == '"'     = transition2 [c]
selectTransition c | c == '\\'    = transition4 [c]
selectTransition c                = transition3 [c]

myDiagram :: Diagram B
myDiagram = hsep 0 (fmap selectTransition jsonText)

illustrateBézier
  :: Semigroup a
  => TrailLike a
  => Transformable a
  => HasStyle a
  => Typeable (N a)
  => V a ~ V2
  => V a (N a)
  -> V a (N a)
  -> V a (N a)
  -> a
illustrateBézier c1 c2 x2
    =  endpt
    <> endpt  # translate x2
    <> ctrlpt # translate c1
    <> ctrlpt # translate c2
    <> l1
    <> l2
    <> fromSegments [bézier3 c1 c2 x2]
  where dashed  = dashingN [0.03, 0.03] 0
        endpt   = circle 0.05 # fc orange  # lw none
        ctrlpt  = circle 0.05 # fc blue # lw none
        l1      = fromOffsets [c1] # dashed
        l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed

smSingleTransition :: Int -> Int -> QDiagram B V2 Double Any
smSingleTransition a b = fromSegments
  [ bézier3
      (r2 (1, 0))
      (r2 (2, fromIntegral (bx - ax)))
      (r2 (3, fromIntegral (bx - ax)))
  ] # translateY (min (fromIntegral bx) (fromIntegral ax) + max (fromIntegral (ax - bx)) 0)
  where ax = 2 * a
        bx = 2 * b

transition0 :: String -> QDiagram B V2 Double Any
transition0 label = mconcat
  [ [ smSingleTransition 3 2 # lc black # lw veryThick
    , smSingleTransition 2 2 # lc black # lw veryThick
    , smSingleTransition 1 0 # lc black # lw veryThick
    , smSingleTransition 0 0 # lc black # lw veryThick
    ] # mconcat # opacityGroup 0.5
  , vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc black
  ]

transition1 :: String -> QDiagram B V2 Double Any
transition1 label = mconcat
  [ [ smSingleTransition 3 2 # lc green # lw veryThick
    , smSingleTransition 2 2 # lc green # lw veryThick
    , smSingleTransition 1 0 # lc green # lw veryThick
    , smSingleTransition 0 0 # lc green # lw veryThick
    ] # mconcat # opacityGroup 0.5
  , vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc green
  ]
  
transition2 :: String -> QDiagram B V2 Double Any
transition2 label = mconcat
  [ [ smSingleTransition 3 2 # lc orange # lw veryThick
    , smSingleTransition 2 0 # lc orange # lw veryThick
    , smSingleTransition 1 2 # lc orange # lw veryThick
    , smSingleTransition 0 2 # lc orange # lw veryThick
    ] # mconcat # opacityGroup 0.5 
  , vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc orange
  ]

transition3 :: String -> QDiagram B V2 Double Any
transition3 label = mconcat
  [ [ smSingleTransition 3 2 # lc brown # lw veryThick
    , smSingleTransition 2 2 # lc brown # lw veryThick
    , smSingleTransition 1 1 # lc brown # lw veryThick
    , smSingleTransition 0 1 # lc brown # lw veryThick
    ] # mconcat # opacityGroup 0.5 
  , vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc brown
  ]

transition4 :: String -> QDiagram B V2 Double Any
transition4 label = mconcat
  [ [ smSingleTransition 3 2 # lc blue # lw veryThick
    , smSingleTransition 2 3 # lc blue # lw veryThick
    , smSingleTransition 1 0 # lc blue # lw veryThick
    , smSingleTransition 0 0 # lc blue # lw veryThick
    ] # mconcat # opacityGroup 0.5 
  , vrule 9 # lc silver # translateY 2
  , vrule 9 # lc silver # translateY 2 # translateX 3
  , text label # translateX 1.5 # translateY (-1.5) # font "Consolas,monaco,monospace" # fc blue
  ]

jsonText :: String
jsonText = "{\"key\": [12, \"[\\\"a\\\"]\"]}"

run :: IO ()
run = do
  IO.createDirectoryIfMissing True "output/hw-json"
  renderSVG "output/hw-json/output.svg" (mkWidth 1024) myDiagram
