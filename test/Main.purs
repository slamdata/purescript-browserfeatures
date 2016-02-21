module Test.Main where

import Prelude (Unit, bind, show, (++), ($), void)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)

import Data.Traversable (for)
import Data.BrowserFeatures.InputType as IT
import DOM (DOM)
import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = do
  queryFeatureSupport
  log "Will query feature support again, using memoized results"
  queryFeatureSupport
  log "Done"

  where
    queryFeatureSupport = do
      features <- detectBrowserFeatures
      void $ for IT.allInputTypes \ty ->
        log $ show ty ++ if features.inputTypeSupported ty then " supported" else " not supported"
