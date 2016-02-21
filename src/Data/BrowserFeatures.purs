module Data.BrowserFeatures
  ( BrowserFeatures(..)
  ) where

import Data.BrowserFeatures.InputType (InputType)

type BrowserFeatures =
  { inputTypeSupported :: InputType -> Boolean
  }
