module Data.Lense.Prism.List (_uncons) where

import Data.Lens.Prism (prism')
import Data.Lens.Types (Prism')
import Data.List (uncons, (:))
import Data.List.Types (List)

_uncons :: forall a. Prism' (List a) ({head:: a, tail:: List a})
_uncons = prism' (\{head, tail} -> head : tail) uncons
