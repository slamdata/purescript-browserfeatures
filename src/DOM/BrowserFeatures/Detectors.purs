module DOM.BrowserFeatures.Detectors
  ( detectBrowserFeatures
  ) where

import Prelude

import Effect (Effect)
import Effect.Exception (catchException)
-- import Control.Monad.Eff.Ref (modifyRef, readRef, newRef)
import Effect.Unsafe as Unsafe
import Effect.Ref (new, read, modify_)

import Data.BrowserFeatures (BrowserFeatures)
import Data.BrowserFeatures.InputType as IT
import Data.Foldable (foldr)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Web.HTML (window) as DOM
import Web.DOM.Internal.Types (Element) as DOM
import Web.HTML.Window as Win
import Web.DOM.Element as Elem
import Web.DOM.Document as Doc

foreign import _getTypeProperty :: DOM.Element -> Effect String

type InputTypeMap = M.Map IT.InputType Boolean

-- | This is safe, because memoization is a monotonic & universally benign
-- | effect.
memoizeEff :: forall i o. (Ord i) => (i -> Effect o) -> i -> Effect o
memoizeEff f =
  Unsafe.unsafePerformEffect $ do
    cacheRef <- new M.empty
    pure \i -> Unsafe.unsafePerformEffect $ do
      cache <- read cacheRef
      case M.lookup i cache of
        Just o -> pure o
        Nothing -> do
          o <- f i
          modify_ (M.insert i o) cacheRef 
          pure o

detectInputTypeSupport :: IT.InputType -> Effect Boolean
detectInputTypeSupport =
  memoizeEff \it -> do
    window <- DOM.window
    document <- ?hole <$> Win.document window
    element <- Doc.createElement "input" document

    let ty = IT.renderInputType it
    catchException (\_ -> pure false) $ do
      Elem.setAttribute "type" ty element
      ty' <- _getTypeProperty element
      pure $ ty == ty'

detectInputTypeSupportMap :: Effect InputTypeMap
detectInputTypeSupportMap = M.fromFoldable <$> traverse (\t -> Tuple t <$> detectInputTypeSupport t) inputTypes
  where
    inputTypes :: L.List IT.InputType
    inputTypes = foldr L.Cons L.Nil IT.allInputTypes

-- | Detect browser features by testing them using the DOM.
detectBrowserFeatures :: Effect BrowserFeatures
detectBrowserFeatures = do
  inputTypeSupportMap <- detectInputTypeSupportMap
  pure { inputTypeSupported : fromMaybe false <<< flip M.lookup inputTypeSupportMap
       }
