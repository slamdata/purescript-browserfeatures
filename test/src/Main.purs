module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.BrowserFeatures.InputType as IT
import Data.Traversable (for)

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

main :: Effect Unit
main = do
  queryFeatureSupport
  log "Will query feature support again, using memoized results"
  queryFeatureSupport
  log "Done"

  where
    queryFeatureSupport = do
      features <- detectBrowserFeatures
      void $ for IT.allInputTypes \ty ->
        log $ show ty <> if features.inputTypeSupported ty then " supported" else " not supported"
