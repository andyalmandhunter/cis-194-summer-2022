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

import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage =
  let go ("I":t:m)   = LogMessage Info (read t :: Int) (unwords m)
      go ("W":t:m)   = LogMessage Warning (read t :: Int) (unwords m)
      go ("E":s:t:m) = LogMessage (Error (read s :: Int)) (read t :: Int) (unwords m)
      go m           = Unknown (unwords m)
  in go . words

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
