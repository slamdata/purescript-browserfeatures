module Data.BrowserFeatures
  ( BrowserFeatures(..)
  ) where

import Data.BrowserFeatures.InputType

type BrowserFeatures =
  { inputTypeSupported :: InputType -> Boolean
  }
