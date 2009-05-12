module Hellnet.Network (fetchChunk, fetchChunks, nodesList, writeNodesList, findChunk, findChunks, findFile) where

import Hellnet.Storage
import System.IO.Error
import Network.HTTP
import Codec.Utils
import Control.Monad
import Hellnet.Utils
import Hellnet
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

findFile :: [Octet] -> IO (Maybe [Octet])
findFile hsh = do
	putStrLn ("Locating file " ++ (hashToHex hsh))
	link <- findChunk hsh
	print link
	res <- maybe (return Nothing) (findFile') link
	return res

findFile' :: [Octet] -> IO (Maybe [Octet])
findFile' cs = do
	putStrLn ("Locating file' " ++ (hashToHex cs))
	let chs = splitFor hashSize cs
	putStrLn ("cs = " ++ (show chs))
	chs' <- findChunks (take hashesPerChunk chs)
	putStrLn ("chs' = " ++ (show chs'))
	maybe (return Nothing)
		(\c -> if (length chs) == (hashesPerChunk + 1) then do
			f <- findFile (last c)
			maybe (return Nothing) (\ff -> return (Just ((foldl1 (++) c) ++ ff)) ) f
			else
			return (Just (foldl1 (++) c))
			) chs'

findChunks :: [[Octet]] -> IO (Maybe [[Octet]])
findChunks chs = do
	res <- mapM (findChunk) chs
	if null (filter ((==) (Nothing)) res) then
		return (Just (map (unjust) res))
		else
		return Nothing

findChunk :: [Octet] -> IO (Maybe [Octet])
findChunk hsh = do
	print ("Locating chunk " ++ (hashToHex hsh))
	gC <- try (getChunk hsh)
	either (const (do
		res <- fetchChunks [hsh]
		if (null res) then
			do
				r <- getChunk hsh
				return (Just r)
		   else do
				return Nothing)
		) (return . Just) gC