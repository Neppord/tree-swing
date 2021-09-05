module Data.Lense.Prism.ListSpec where

import Data.Lens.Fold (preview)
import Data.Lense.Prism.List (_uncons)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Control.Bind (discard)

spec :: Spec Unit
spec = describe "_uncons" do
    it "has nothing when the list is empty" do
       preview _uncons (Nil::List Int) `shouldEqual` Nothing
    it "has something when the list has a tail" do
       preview _uncons (1:Nil) `shouldEqual` Just {head: 1, tail: Nil}
