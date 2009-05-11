module Hellnet.Network (nodesList) where

import Hellnet.Storage
import System.IO.Error

nodesList :: IO [(String, Int)]
nodesList = do
	listfile <- try readNodesList
	let getList = (either (const []) (read) listfile) :: [(String, Int)]
	return getList

readNodesList = readFile =<< toFullPath "nodelist"