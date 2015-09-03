module Test.Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)

import Data.Traversable (traverse)
import qualified Data.BrowserFeatures.InputType as IT
import DOM
import DOM.BrowserFeatures.Detectors

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = do
  features <- detectBrowserFeatures
  void $ flip traverse IT.allInputTypes \ty ->
    log $ show ty ++ if features.inputTypeSupported ty then " supported" else " not supported"
