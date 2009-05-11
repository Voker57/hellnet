module Hellnet.Network (fetchChunk, fetchChunks, nodesList, writeNodesList, findChunk) where

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
fetchChunks :: [[Octet]] -> IO [[Octet]]
fetchChunks cs = do
	nodes <- nodesList
	let fs = map (fetchChunksFromNode) nodes
	nps <- filtM fs cs
	return nps

fetchChunksFromNode :: Node -> [[Octet]] -> IO [[Octet]]
fetchChunksFromNode s cs = filterM (fetchChunk' s) cs

fetchChunk' :: Node -> [Octet] -> IO Bool
fetchChunk' s p = do
	b <- fetchChunk s p
	return (not b)

-- | retrieves chunk using server, returns success status
fetchChunk :: Node -> [Octet] -> IO Bool
fetchChunk s p = do
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

findChunk :: [Octet] -> IO (Maybe [Octet])
findChunk hsh = do
	res <- fetchChunks [hsh]
	if (null res) then
		do
			r <- getChunk hsh
			return (Just r)
		else do
			return Nothing