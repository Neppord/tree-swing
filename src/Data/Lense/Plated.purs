module Data.Lens.Plated
    ( class Plated
    , plate
    , childern
    , rewrite
    , rewriteOf
    , transformOf
    ) where

import Prelude

import Data.Lens.Fold (toListOf)
import Data.Lens.Record (prop)
import Data.Lens.Setter (over)
import Data.Lens.Types (Setter, Traversal')
import Data.Lense.Prism.List (_uncons)
import Data.List.Types (List)
import Data.Maybe (Maybe, maybe)
import Type.Proxy (Proxy(..))

class Plated a where
    plate :: Traversal' a a

instance listPlated :: Plated (List a) where
    plate = _uncons <<< prop (Proxy :: Proxy "tail")

childern :: forall a. Plated a => a -> List a
childern = toListOf plate

rewrite :: forall a. Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate

rewriteOf :: forall a b. Setter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf plates f = go
    where go = transformOf plates (\x -> maybe x go (f x))

transformOf :: forall a b. Setter a b a b -> (b -> b) -> a -> b
transformOf plates f = go
    where
        go s = s
            # over plates (\ s' -> go s')
            # f