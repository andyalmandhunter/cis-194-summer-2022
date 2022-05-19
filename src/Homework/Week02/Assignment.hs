module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Text.Read
import Homework.Week02.Log

-- #1a
newLogMessage :: Maybe MessageType -> Maybe Int -> [String] -> String -> LogMessage
newLogMessage _ Nothing _ msg        = Unknown msg
newLogMessage Nothing _ _ msg        = Unknown msg
newLogMessage (Just mt) (Just t) m _ = LogMessage mt t (unwords m)

newError :: Maybe Int -> Maybe MessageType
newError Nothing  = Nothing
newError (Just n) = Just (Error n)

parseMessage :: String -> LogMessage
parseMessage msg =
  let go ("I":t:m)   = newLogMessage (Just Info) (readMaybe t :: Maybe Int) m msg
      go ("W":t:m)   = newLogMessage (Just Warning) (readMaybe t :: Maybe Int) m msg
      go ("E":s:t:m) = newLogMessage (newError (readMaybe s :: Maybe Int)) (readMaybe t :: Maybe Int) m msg
      go _           = Unknown msg
  in go (words msg)

-- #1b
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert l Leaf         = Node Leaf l Leaf
insert l@(LogMessage _ t _) (Node a ll@(LogMessage _ tt _) b)
  | t < tt    = Node (insert l a) ll b
  | otherwise = Node a ll (insert l b)

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node a l b) = (inOrder a) ++ [l] ++ (inOrder b)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = [m | LogMessage (Error s) _ m <- inOrder (build ms), s >= 50]
