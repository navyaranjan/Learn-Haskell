module LogAnalysis where

import Log

--splitting words in message to list of words and parsing by patteren matching
parseMessage :: String -> LogMessage
parseMessage message=case wordList of
                        ("E":sev:time:msgString)->LogMessage (Error (read sev)) (read time) (unwords msgString)
                        ("I":time:msgString)    ->LogMessage Info (read time) (unwords msgString)
                        ("W":time:msgString)    ->LogMessage Warning (read time) (unwords msgString)
                        _                       ->Unknown message
      where wordList=words message  
      
--to parse a log file
parse :: String -> [LogMessage]
parse str=[parseMessage message | message <- (lines str)]

--getter function for timestamp
getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ t _ )=t

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
inOrder Leaf        =[]
inOrder (Node l msg r)=inOrder(l)++[msg]++inOrder(r)

--extracting error messages with severity atleast 50
whatWentWrong :: [LogMessage]->[String]
whatWentWrong messageList = [errorMsg | (LogMessage msgType time errorMsg)<-sortedList messageList, checkType msgType]
                   where sortedList list   = inOrder (build (list))
                         checkType msgType = case msgType of
                                             Error severity -> severity >=50 
                                             _              -> False
                                             