module Hellnet.Network (nodesList, writeNodesList) where

import Hellnet.Storage
import System.IO.Error

type Node = (String, Int)

nodesList :: IO [Node]
nodesList = do
	listfile <- try readNodesList
	return (either (const []) (read) listfile)

readNodesList = readFile =<< toFullPath "nodelist"

writeNodesList :: [Node] -> IO ()
writeNodesList ns = do
	fp <- toFullPath "nodelist"
	writeFile fp (show ns)