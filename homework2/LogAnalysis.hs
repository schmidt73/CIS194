{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

getInt :: Int -> String -> Int
getInt n s = read ((words s) !! n) :: Int

parseMessage :: String -> LogMessage
parseMessage ('E': ' ' : s) = LogMessage (Error (getInt 0 s)) (getInt 1 s) (unwords (drop 2 (words s)))
parseMessage ('I': ' ' : s) = LogMessage Info (getInt 0 s) (unwords (drop 1 (words s)))
parseMessage ('W': ' ' : s) = LogMessage Warning (getInt 0 s) (unwords (drop 1 (words s)))
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map (parseMessage) (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert iMsg@(LogMessage _ iTime _) (Node a msg@(LogMessage _ time _) c)
     | iTime < time && a == Leaf = Node (Node Leaf iMsg Leaf) msg c
     | iTime < time              = Node (insert iMsg a) msg c
     | iTime > time && c == Leaf = Node a msg (Node Leaf iMsg Leaf)
     | iTime > time              = Node a msg (insert iMsg c)
insert _ a = a

build :: [LogMessage] -> MessageTree
build [msg] = Node Leaf msg Leaf
build msgs  = insert (last msgs) (build (init msgs))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                 = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node n1 msg n2)     = (inOrder n1) ++ [msg] ++ (inOrder n2)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgs = let msg = head msgs
                     in case msg of 
      (LogMessage (Error val) _ str) -> if val >= 50 then (str : whatWentWrong (tail msgs))
                                        else whatWentWrong (tail msgs)
      _ -> whatWentWrong (tail msgs)

