module Hellnet.Network (nodesList) where

import Hellnet.Storage
import System.IO.Error

nodesList :: IO [(String, Int)]
nodesList = do
	listfile <- try readNodesList
	return (either (const []) (read) listfile)

readNodesList = readFile =<< toFullPath "nodelist"