{-# OPTIONS_GHC -Wall #-}

module HW12.RiskSpec
  ( main
  , spec
  ) where

import HW12.Risk (computeLoss)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "HW12" $
    describe "attackResults" $ do
      it "should have no effect on attack if there are no dices" $ computeLoss [] [] `shouldBe` (0, 0)
      it "should have no effect on attack if one player has no dices" $ do
        computeLoss [] [1] `shouldBe` (0, 0)
        computeLoss [1] [] `shouldBe` (0, 0)
      it "should have less defenders if attacker roll a higher number" $ computeLoss [6] [1] `shouldBe` (0, 1)
      it "should have less attackers if defenders roll a higher number" $ computeLoss [1] [6] `shouldBe` (1, 0)
      it "should have less attackers if there is a tie" $ computeLoss [6] [6] `shouldBe` (1, 0)
      it "should have two less defenders if attacker rolls two higher values" $
        computeLoss [5, 6] [2, 3] `shouldBe` (0, 2)
      it
        "should have one less defenders and one less attacker if attacker a higher roll on one pair, but defender has a higher roll on another" $
        computeLoss [3, 6] [4, 5] `shouldBe` (1, 1)
      it "should have two less attackers if defender rolls two higher or equla values" $
        computeLoss [4, 5] [5, 6] `shouldBe` (2, 0)
