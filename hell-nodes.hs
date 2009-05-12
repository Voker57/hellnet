import Hellnet.Network
import System.Environment
import Hellnet.Storage
import Control.Exception (evaluate)

node ss = ( (ss !! 1), (read (ss !! 2)) :: Int )

main = do
	nodes <- nodesList
	evaluate nodes
	args <- getArgs
	if and [((length args) == 3), ((head args) == "add")] then
		writeNodesList ((node args) : nodes)
		else if and [((length args) == 3), ((head args) == "rm")] then
			writeNodesList (dropWhile (== (node args)) nodes)
			else if and [((length args) == 1), ((head args) == "clear")] then
				writeNodesList []
				else if and [((length args) == 1), ((head args) == "list")] then
					print nodes
					else
					putStrLn "Usage: hell-nodes {add,rm,clear,list} <host> <port>"