--------------------------------------------------------------------------------
--     This file is part of Hellnet
--
--     Hellnet is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.
--
--     Hellnet is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
--
--     You should have received a copy of the GNU General Public License
--     along with Hellnet.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

module Hellnet.Network (fetchChunk, fetchChunks, nodesList, writeNodesList, findChunk, findChunks, findFile, fetchFile, findHashesByMetaFromNode, findHashesByMetaFromNodes, findChunksByMeta, findHashesByMeta) where

import Codec.Utils
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import Hellnet
import Hellnet.Storage
import Hellnet.Utils
import Network.HTTP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.ByteString.Lazy as BSL
import Random
import System.IO.Error

type Node = (String, Int)

nodesList :: IO [Node]
nodesList = do
	listfile <- try readNodesList
	return (either (const []) (read) listfile)

readNodesList = readFile =<< toFullPath "nodelist"

writeNodesList :: [Node] -> IO ()
writeNodesList ns = do
	storeFile "nodelist" $ BS8.pack $ show ns

-- | retrieves pieces using servers node list and returns list of pieces that are unavailable.
fetchChunks :: [Hash] -> IO [Hash]
fetchChunks cs = do
	nodes <- nodesList
	decoys <- mapM (const genHash) [0..(((length cs) `div` 3) + 1)]
	let decoys' = decoys \\ cs
	chunks <- shuffle (cs ++ decoys')
	let todos = splitFor getThreads chunks
	workers <- mapM (forkChild) $ map (fetchChunksFromNodes nodes) todos
	nps <- mapM (takeMVar) workers
	return ((concat nps) \\ decoys')

-- | for usage in workers
fetchChunksFromNodes :: [Node] -> [Hash] -> IO [Hash]
fetchChunksFromNodes ns cs = do
	nodes <- shuffle ns
	let fs = map (fetchChunksFromNode) ns
	return =<< filtM fs cs

fetchChunksFromNode :: Node -> [Hash] -> IO [Hash]
fetchChunksFromNode s cs = filterM (fetchChunk' s) cs

fetchChunk' :: Node -> Hash -> IO Bool
fetchChunk' s p = do
	b <- fetchChunk s p
	return (not b)

-- | retrieves chunk using server, returns success status
fetchChunk :: Node -> Hash -> IO Bool
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
					chID <- Hellnet.Storage.insertChunk Nothing $ BS.unpack $ BS8.pack (rspBody r)
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

-- | find file, returns either not found chunks or file in lazy ByteString
findFile :: Maybe Key -> Hash -> IO (Either [Hash] BSL.ByteString)
findFile key hsh = do
	link <- findChunk key hsh
	maybe (return (Left [hsh])) (\l -> do
		res <- findFile' key (BS.unpack l)
		return res
		) link

findFile' :: Maybe Key -> Chunk -> IO (Either [Hash] BSL.ByteString)
findFile' key cs = do
	let chs = splitFor hashSize cs
	chs' <- findChunks key chs
	either (return . Left)
		(\c -> if (length chs) == (hashesPerChunk + 1) then do
			f <- findFile' key (BS.unpack (last c))
			either (return . Left) (\ff -> return $ Right $ BSL.concat [(BSL.pack $ BS.unpack $ BS.concat $ init c), ff]) f
			else
			return (Right (BSL.pack (BS.unpack (BS.concat c))))
		) chs'

-- | tries to locate chunks, returns either list of unavailable ones or list of chunks' content
findChunks :: Maybe Key -> [Hash] -> IO (Either [Hash] [BS.ByteString])
findChunks key chs = do
	res <- mapM (getChunk key) chs
	let unavailable = map (fst) (filter ((== Nothing) . snd) (zip chs res))
	if null unavailable then
		return (Right (catMaybes res))
		else do
			fromNet <- fetchChunks unavailable
			if null fromNet then do
				chs' <- mapM (getChunk key) chs
				return (Right (catMaybes chs'))
				else
				return (Left fromNet)

findChunk :: Maybe Key -> Hash -> IO (Maybe BS.ByteString)
findChunk key hsh = do
	fC <- findChunks key [hsh]
	either (const (return Nothing)) (return . Just . head) fC

-- | prepares file for collecting from storage, returns either list of unavailable chunks or unrolled filelink
fetchFile :: Maybe Key -> Hash -> IO (Either [Hash] [Hash])
fetchFile encKey hsh = do
	link <- findChunk encKey hsh
	maybe (return (Left [hsh])) (\l -> do
		res <- fetchFile' encKey (BS.unpack l)
		return res
		) link

fetchFile' :: Maybe Key -> Chunk -> IO (Either [Hash] [Hash])
fetchFile' encKey cs = do
	let chs = splitFor hashSize cs
	stored <- filterM (isStored) chs
	chs' <- if stored /= chs then
		fetchChunks chs
		else
		return []
	if	not (null chs') then do
		return (Left chs')
		else
		if (length chs) == (hashesPerChunk + 1) then do
			c <- getChunk encKey $ last chs
			f <- fetchFile' encKey $ BS.unpack $ unjust $ c
			return (either (Left) (Right . ((++) (init chs))) f)
			else
			return (Right chs)

findHashesByMetaFromNode :: Meta -> Node -> IO [Hash]
findHashesByMetaFromNode (Meta key value) node = do
	let reqString = "http://" ++ (fst node) ++ ":" ++ (show $ snd node)
		++ "/meta/" ++ key ++ "/" ++ value
	catch (do
		rep <- simpleHTTP (getRequest reqString)
		return $ either (const []) (\rsp -> if (rspCode rsp) == (2,0,0) then
			splitFor hashSize $ BS.unpack $ BS8.pack $ rspBody rsp
			else
			[]) rep
		) (const $ return [])

findHashesByMetaFromNodes :: Meta -> [Node] -> IO [Hash]
findHashesByMetaFromNodes m nodes = do
	hs <- mapM (findHashesByMetaFromNode m) nodes
	return $ nub $ concat hs

findChunksByMeta :: (Maybe Key) -> Meta -> IO [Chunk]
findChunksByMeta encKey m = do
	return =<< return . map (BS.unpack) . catMaybes =<< mapM (getChunk encKey) =<< findHashesByMeta m

findHashesByMeta :: Meta -> IO [Hash]
findHashesByMeta m = do
	nodes <- shuffle =<< nodesList
	let nodeSets = splitFor getThreads nodes
	workers <- mapM (forkChild) $ map (findHashesByMetaFromNodes m) nodeSets
	ress <- mapM (takeMVar) workers
	addHashesToMeta m $ (nub . concat) ress
	return =<< getHashesByMeta m