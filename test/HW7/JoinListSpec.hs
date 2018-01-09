{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}


module HW7.JoinListSpec (main, spec) where

import Test.Hspec (hspec, Spec, describe, it, shouldBe)
import Test.QuickCheck

import qualified Test.QuickCheck.Gen as G

import Data.Monoid (Product(..), (<>))

import HW7.JoinList
import HW7.Sized
import HW7.Scrabble
import HW7.Buffer


main :: IO ()
main = hspec spec

computeAttribute :: Monoid m => JoinList m a -> m
computeAttribute Empty = mempty
computeAttribute (Single m _) = m
computeAttribute (Append _ left right) = mappend (computeAttribute left) (computeAttribute right)

generateJoinList :: (Arbitrary m, Arbitrary a, Monoid m) => Gen m -> Gen (JoinList m a)
generateJoinList monoidGenerator =  frequency [
                                    (1, return Empty),
                                    (2, Single <$> monoidGenerator <*> arbitrary),
                                    (3, (+++) <$> generateJoinList monoidGenerator <*> generateJoinList monoidGenerator)
                                ]

instance (Arbitrary m, Arbitrary a, Monoid m) => Arbitrary (JoinList m a) where
    arbitrary = generateJoinList arbitrary

instance Arbitrary Size where
    arbitrary = Size <$> arbitrary

sizedJoinListGenerator :: Gen (JoinList Size Integer)
sizedJoinListGenerator = generateJoinList (elements [Size 1])


testPairSizedList :: (JoinList Size Integer -> Int -> Bool)-> Property
testPairSizedList joinListPredicte = forAll sizedJoinListGenerator $
    \joinList (Positive n) ->  (joinList `joinListPredicte` n)

spec :: Spec
spec = describe "HW7" $ do
    it "should multiple numbers correctly" $ property $
        \joinList -> computeAttribute joinList == tag (joinList :: JoinList (Product Integer) String)

    it "should have join-list's index function be similar to lists" $ testPairSizedList
        (\joinList index -> indexJ index joinList == (jlToList joinList !!? index))

    it "should have join-list's drop function be similar to list's" $ testPairSizedList
        (\joinList n -> jlToList (dropJ n joinList) == drop n (jlToList joinList))

    it "should have join-list's take function be similar to list's" $ testPairSizedList
        (\joinList n -> jlToList (takeJ n joinList) == take n (jlToList joinList))

    it "should scoreLine (word1 ++ word2) = scoreLine(word1) + scoreLine(word2) for every word1, word2 in string" $ property $
        \word1 word2 -> tag (scoreLine word1 +++ scoreLine word2) == tag ( scoreLine $ word1 ++ word2)

    it "should compute scrabble words correctly" $ do
        scoreString "john" `shouldBe` Score 14
        scoreString "the quick brown fox jumps over the lazy dog" `shouldBe` Score 97

    it "should correctly create a Join List from a string" $ property (\text ->
            let joinList = fromString text in
            snd ( tag (joinList :: JoinList (Score, Size) String)) == (Size $ length $ lines text)
        )
