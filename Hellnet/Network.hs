module Hellnet.Network (retrieveChunk, nodesList, writeNodesList) where

import Hellnet.Storage
import System.IO.Error
import Network.HTTP
import Codec.Utils
import Control.Monad
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
retrieveChunks :: [[Octet]] -> IO [[Octet]]
retrieveChunks cs = do
	nodes <- nodesList
	let fs = map (retrieveChunksFromNode) nodes
	nps <- filtM fs cs
	return nps

retrieveChunksFromNode :: Node -> [[Octet]] -> IO [[Octet]]
retrieveChunksFromNode s cs = filterM (retrieveChunk' s) cs

retrieveChunk' :: Node -> [Octet] -> IO Bool
retrieveChunk' s p = do
	b <- retrieveChunk s p
	return (not b)

-- | retrieves chunk using server, returns success status
retrieveChunk :: Node -> [Octet] -> IO Bool
retrieveChunk s p = do
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
					if (chID == p) then
						return True
						else
						do
							Hellnet.Storage.purgeChunk chID
							return False
				else
				return False
			) resp)
		)
		(\ _ -> return False)
