module HW8.PartySpec
  ( main
  , spec
  ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Arbitrary, arbitrary, frequency, elements, forAll, vectorOf, Gen, oneof, choose)
import Data.Tree

import HW8.Party (treeFold)

main :: IO ()
main = hspec spec

treeGenerator :: Arbitrary a => Int -> Gen (Tree a)
treeGenerator branchFactor = do
  treeSize <- oneof [choose (1, branchFactor), elements [0]]
  Node <$> arbitrary <*> vectorOf treeSize (treeGenerator branchFactor)

spec :: Spec
spec = describe "HW8" $
  it "should have treeFold equivalent to fold for lists" $ forAll (treeGenerator 2) (\tree ->
      treeFold (\value list -> value + sum list) (tree :: Tree Integer) == sum (flatten tree)
    )
