module DOM.BrowserFeatures.Detectors
  ( detectBrowserFeatures
  ) where

import Prelude

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Exception (catchException)
import Control.Monad.Eff.Ref (modifyRef, readRef, newRef)
import Control.Monad.Eff.Unsafe as Unsafe

import Data.BrowserFeatures (BrowserFeatures)
import Data.BrowserFeatures.InputType as IT
import Data.Foldable (foldr)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window as Win
import DOM.Node.Document as Doc
import DOM.Node.Element as Elem
import DOM.Node.Types (Element) as DOM

foreign import _getTypeProperty :: forall e. DOM.Element -> Eff (dom :: DOM | e) String

type InputTypeMap = M.Map IT.InputType Boolean

-- | This is safe, because memoization is a monotonic & universally benign
-- | effect.
memoizeEff :: forall i e o. (Ord i) => (i -> Eff e o) -> i -> Eff e o
memoizeEff f =
  runPure <<< Unsafe.unsafeInterleaveEff $ do
    cacheRef <- newRef M.empty
    pure \i -> Unsafe.unsafeInterleaveEff $ do
      cache <- readRef cacheRef
      case M.lookup i cache of
        Just o -> pure o
        Nothing -> do
          o <- Unsafe.unsafeInterleaveEff $ f i
          modifyRef cacheRef (M.insert i o)
          pure o

detectInputTypeSupport :: forall e. IT.InputType -> Eff (dom :: DOM | e) Boolean
detectInputTypeSupport =
  memoizeEff \it -> do
    window <- DOM.window
    document <- DOM.htmlDocumentToDocument <$> Win.document window
    element <- Doc.createElement "input" document

    let ty = IT.renderInputType it
    catchException (\_ -> pure false) $ do
      Elem.setAttribute "type" ty element
      ty' <- _getTypeProperty element
      pure $ ty == ty'

detectInputTypeSupportMap :: forall e. Eff (dom :: DOM | e) InputTypeMap
detectInputTypeSupportMap = M.fromList <$> traverse (\t -> Tuple t <$> detectInputTypeSupport t) inputTypes
  where
    inputTypes :: L.List IT.InputType
    inputTypes = foldr L.Cons L.Nil IT.allInputTypes

-- | Detect browser features by testing them using the DOM.
detectBrowserFeatures :: forall e. Eff (dom :: DOM | e) BrowserFeatures
detectBrowserFeatures = do
  inputTypeSupportMap <- detectInputTypeSupportMap
  pure { inputTypeSupported : fromMaybe false <<< flip M.lookup inputTypeSupportMap
       }
