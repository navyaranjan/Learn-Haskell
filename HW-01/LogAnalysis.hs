module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
--splitting words in message to list of words and parsing by patteren matching
--read x::Int used for converting String type to Int
parseMessage message
                    |(wordList !! 0) == "E" = LogMessage (Error ((read (wordList !! 1))::Int)) ((read (wordList !! 2))::Int) (unwords (drop 3 wordList))
                    |(wordList !! 0) == "I" = LogMessage Info (read (wordList !! 1) :: Int) (unwords (drop 2 wordList))
                    |(wordList !! 0) == "W" = LogMessage Warning (read (wordList !! 1) :: Int) (unwords (drop 2 wordList))
                    |otherwise              = Unknown message
      where wordList=words message

--to parse a log file
parse :: String -> [LogMessage]
parse str=[parseMessage message | message <- (lines str)]

--getter functions on MessageTree,LogMessage and MessageType
getMessage :: MessageTree-> LogMessage
getTimeStamp :: LogMessage -> TimeStamp
getErrorString :: LogMessage -> String
getSeverity :: MessageType -> Int
getMessageType :: LogMessage -> MessageType

getMessage (Node _ msg _) = msg
getTimeStamp (LogMessage _ t _ )=t
getErrorString (LogMessage _ _ str)=str
getSeverity (Error value)=value
getMessageType (LogMessage err _ _)=err

--to insert LogMessages into MessageTree
insert::LogMessage -> MessageTree -> MessageTree

insert message Leaf= Node Leaf message Leaf
insert message (Node l msg r)
                             |getTimeStamp message >= getTimeStamp msg = Node l msg (insert message r)
                             |otherwise                                = Node (insert message l) msg r

--build a complete MessageTree from List of messages
build ::[LogMessage] -> MessageTree

build (message:[])         = insert message Leaf
build (message:messageList)= insert message (build messageList)

--inorder traversal for MessageTree
inOrder :: MessageTree -> [LogMessage]

inOrder (Leaf)        =[]
inOrder (Node l msg r)=inOrder(l)++[msg]++inOrder(r)

--extracting error messages with severity atleast 50
whatWentWrong :: [LogMessage]->[String]
whatWentWrong messageList = [getErrorString x| x<-sortedList messageList , getMessageType x/=Info && getMessageType x /= Warning && (getSeverity (getMessageType x))>=50]
                   where sortedList x=inOrder (build (messageList))
