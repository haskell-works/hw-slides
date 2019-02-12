module HaskellWorks.Prezzo.Text where

import Diagrams.Backend.SVG
import Diagrams.Prelude

highlightText :: String -> String -> Diagram B
highlightText s selection = hsep 0 $ text . (:[]) <$> s
