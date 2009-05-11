module Hellnet.Network (nodesList, writeNodesList) where

import Hellnet.Storage
import System.IO.Error
import Network.HTTP
import Codec.Utils
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

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

-- | retrieves pieces using servers node list and returns list of pieces that are unavailable.
retrievePieces :: [[Octet]] -> IO [[Octet]]
retrievePieces ps = undefined
--  do
-- 	nodes <- nodesList
-- 	mapM retrievePiece ps

-- | retrieves piece using server, returns success status
retrievePiece :: Node -> [Octet] -> IO Bool
retrievePiece s p = do
	let chunkID = hashToHex p
	let reqString = "http://" ++ (fst s) ++ ":" ++ (show (snd s)) ++ "/chunks/" ++ (take 2 chunkID) ++ "/" ++ (drop 2 chunkID)
	let req = simpleHTTP (getRequest reqString)
	catch (do
		resp <- req
		(either
			(const (return False))
			(\r -> if (rspCode r) == (2,0,0) then
				do
					chID <- Hellnet.Storage.insertChunk (BS.unpack (BS8.pack (rspBody r)))
					return True
				else
				return False
			) resp)
		)
		(\ _ -> return False)
