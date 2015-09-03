module DOM.BrowserFeatures.Detectors
  ( detectBrowserFeatures
  ) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception

import qualified Data.Array as Arr
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Foldable (foldr)
import Data.Traversable (traverse)
import Data.Tuple

import DOM
import Data.BrowserFeatures
import qualified Data.BrowserFeatures.InputType as IT

foreign import _detectInputTypeSupport :: forall e. String -> Eff (dom :: DOM | e) Boolean

detectInputTypeSupport :: forall e. IT.InputType -> Eff (dom :: DOM | e) Boolean
detectInputTypeSupport = _detectInputTypeSupport <<< IT.renderInputType

detectInputTypeSupportMap :: forall e. Eff (dom :: DOM | e) (M.Map IT.InputType Boolean)
detectInputTypeSupportMap = M.fromList <$> traverse (\t -> Tuple t <$> detectInputTypeSupport t) inputTypes
  where
    inputTypes :: L.List IT.InputType
    inputTypes = foldr L.Cons L.Nil IT.allInputTypes

-- | Detect browser features by testing them using the DOM.
detectBrowserFeatures :: forall e. Eff (dom :: DOM | e) BrowserFeatures
detectBrowserFeatures = do
  inputTypeSupportMap <- detectInputTypeSupportMap
  pure { inputTypeSupported : maybe false id <<< flip M.lookup inputTypeSupportMap
       }

