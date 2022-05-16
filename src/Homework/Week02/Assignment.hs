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
  let go ("I":t:m) = LogMessage Info (read t :: Int) (unwords m)
      go ("W":t:m) = LogMessage Warning (read t :: Int) (unwords m)
      go ("E":s:t:m) = LogMessage (Error (read s :: Int)) (read t :: Int) (unwords m)
      go m = Unknown (unwords m)
  in go . words

-- #1b
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
