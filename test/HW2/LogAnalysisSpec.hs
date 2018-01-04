module HW2.LogAnalysisSpec
  ( main
  , spec
  ) where

import HW2.Log
import HW2.LogAnalysis

import Test.Hspec
  ( Spec
  , anyException
  , describe
  , hspec
  , it
  , shouldBe
  , shouldThrow
  )

import Test.QuickCheck
import Data.List (sort)

instance Arbitrary LogMessage where
    arbitrary = do
        errorIntensity <- choose (1, 100)
        messageType <- elements [Error errorIntensity, Warning, Info]
        timeStamp <- arbitrary
        message <- arbitrary
        return $ LogMessage messageType timeStamp message

checkTreeOrder :: MessageTree -> Bool
checkTreeOrder messageTree = checkTreeOrderWithBounds messageTree minBound maxBound where
    checkTreeOrderWithBounds Leaf _ _ = True
    checkTreeOrderWithBounds (Node  l (LogMessage _ timeStamp _) r) minTimeValue maxTimeValue =
        let isValidTimeStamp = minTimeValue <= timeStamp && timeStamp <= maxTimeValue
            leftSubResult = checkTreeOrderWithBounds l minTimeValue timeStamp
            rightSubResult = checkTreeOrderWithBounds r timeStamp maxTimeValue in
        isValidTimeStamp && leftSubResult && rightSubResult

checkInOrder :: [LogMessage] -> Bool
checkInOrder logMessages = let timeStamps = map getTimeStamp logMessages in
    sort timeStamps == timeStamps


createNode :: Int -> LogMessage
createNode n = LogMessage Info n "hello"


main :: IO ()
main = hspec spec

treeGenerator :: Gen MessageTree
treeGenerator = do
    treeSize <- choose (0, 20)
    logMessages <- vector treeSize
    return $ build logMessages

sortedMessageGenerator :: Gen [LogMessage]
sortedMessageGenerator = do
    tree <- treeGenerator
    return $ inOrder tree

spec :: Spec
spec =
  describe "LogAnalysis" $ do
    describe "parser" $ do
      it "should parse error messages correctly" $
        parseMessage "E 2 562 help help" `shouldBe`
        LogMessage (Error 2) 562 "help help"

      it "should parse info messages correctly" $
        parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

      it "should treat unparsable log messages as unknowns" $
        parseMessage "This is not in the right format" `shouldBe`
        Unknown "This is not in the right format"

    describe "message tree" $ do
      it "should never have a tree change state when an Unknown type is inserted" $
        insert (Unknown "Hello") Leaf `shouldBe` Leaf

      it "should have a list of nodes inserted into a BST" $ property $ forAll treeGenerator checkTreeOrder

      it "should sort a list of nodes based on timeStamp" $ property $ forAll sortedMessageGenerator checkInOrder



-- foldl (<->) Leaf [createNode 8, createNode 10]
