module DOM.BrowserFeatures.Detectors
  ( detectBrowserFeatures
  ) where

import Prelude
import Control.Monad.Eff
import qualified Control.Monad.Eff.Unsafe as Unsafe
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldr)
import Data.Traversable (traverse)
import Data.Tuple

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.Node.Types as DOM
import qualified DOM.HTML.Window as Win
import qualified DOM.Node.Document as Doc
import qualified DOM.Node.Element as Elem
import Data.BrowserFeatures
import qualified Data.BrowserFeatures.InputType as IT

foreign import _getTypeProperty :: forall e. DOM.Element -> Eff (dom :: DOM.DOM | e) String

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

detectInputTypeSupport :: forall e. IT.InputType -> Eff (dom :: DOM.DOM | e) Boolean
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

detectInputTypeSupportMap :: forall e. Eff (dom :: DOM.DOM | e) InputTypeMap
detectInputTypeSupportMap = M.fromList <$> traverse (\t -> Tuple t <$> detectInputTypeSupport t) inputTypes
  where
    inputTypes :: L.List IT.InputType
    inputTypes = foldr L.Cons L.Nil IT.allInputTypes

-- | Detect browser features by testing them using the DOM.
detectBrowserFeatures :: forall e. Eff (dom :: DOM.DOM | e) BrowserFeatures
detectBrowserFeatures = do
  inputTypeSupportMap <- detectInputTypeSupportMap
  pure { inputTypeSupported : fromMaybe false <<< flip M.lookup inputTypeSupportMap
       }

