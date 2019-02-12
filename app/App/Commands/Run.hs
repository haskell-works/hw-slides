{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Run
  ( cmdRun
  ) where

import App.Commands.Types
import HaskellWorks.Prezzo.Slides
import Options.Applicative        hiding (columns)

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runRun :: RunOptions -> IO ()
runRun _ = run

optsRun :: Parser RunOptions
optsRun = pure RunOptions

cmdRun :: Mod CommandFields (IO ())
cmdRun = command "run"  $ flip info idm $ runRun <$> optsRun
