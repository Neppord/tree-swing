module Data.Lense.PlatedSpec where

import Data.Lens.Plated

import Control.Bind (discard)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Prelude (($), (+))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Plated" do
    describe "childern for List" do
        it "has no children if empty" do
            childern (Nil:: List Int) `shouldEqual` Nil
        it "has the tail children if not empty" do
            childern (1:Nil) `shouldEqual` (Nil:Nil)
    describe "rewrite on list" do
        it "rewrites nothing with a nothing rule" do
            let
                rule _ = Nothing
            rewrite rule (1:Nil) `shouldEqual` (1:Nil)
        it "rewrites untill it cant apply the rule any more" do
            let
                rule (Cons a (Cons b tail)) = Just $ (a + b) : tail
                rule _ = Nothing
            rewrite rule (1:2:3:Nil) `shouldEqual` (6:Nil)