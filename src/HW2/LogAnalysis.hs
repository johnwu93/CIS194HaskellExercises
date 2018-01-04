module HW2.LogAnalysis where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import HW2.Log

parseMessage :: String -> LogMessage
parseMessage message = fromMaybe (Unknown message) result
  where
    result = do
      parsedMessages <- Just (words message)
      (messageType, remainingMessages) <- parseMessageType parsedMessages
      (timeStamp, logMessages) <- parseTimeStamp remainingMessages
      return $ LogMessage messageType timeStamp $ unwords logMessages

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType ("I":tail) = Just (Info, tail)
parseMessageType ("W":tail) = Just (Warning, tail)
parseMessageType ("E":maybeSeverity:tail) = do
  severity <- readMaybe maybeSeverity
  return (Error severity, tail)
parseMessageType _ = Nothing

parseTimeStamp :: [String] -> Maybe (TimeStamp, [String])
parseTimeStamp parsedMessages =
  case parsedMessages of
    maybeTimestamp:logMessage ->
      readMaybe maybeTimestamp >>= \timeStamp -> Just (timeStamp, logMessage)
    _ -> Nothing

parseMessages :: String -> [LogMessage]
parseMessages messages = map parseMessage $ lines messages

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert insertedLogMessage@(LogMessage _ insertedTimeStamp _) (Node left currLogMessage@(LogMessage _ currentTimeStamp _) right) =
  if insertedTimeStamp <= currentTimeStamp
    then Node (insert insertedLogMessage left) currLogMessage right
    else Node left currLogMessage (insert insertedLogMessage right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l logMessage r) = inOrder l ++ logMessage : inOrder r


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = let filteredMessages = filter ((> 50) . getTimeStamp) logMessages in
    map getMessage filteredMessages
