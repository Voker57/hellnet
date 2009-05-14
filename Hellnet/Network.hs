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

module Hellnet.Network (fetchChunk, fetchChunks, nodesList, writeNodesList, findChunk, findChunks, findFile) where

import Hellnet.Storage
import System.IO.Error
import Network.HTTP
import Codec.Utils
import Control.Monad
import Hellnet.Utils
import Hellnet
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 (pack)
import Random

type Node = (String, Int)

nodesList :: IO [Node]
nodesList = do
	listfile <- try readNodesList
	return (either (const []) (read) listfile)

readNodesList = readFile =<< toFullPath "nodelist"

writeNodesList :: [Node] -> IO ()
writeNodesList ns = do
	storeFile "nodelist" (BS8.pack (show ns))

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
					chID <- Hellnet.Storage.insertChunk (BS8.pack (rspBody r))
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

findFile :: [Octet] -> IO (Either [[Octet]] BS.ByteString)
findFile hsh = do
	link <- findChunk hsh
	maybe (return (Left [hsh])) (\l -> do
		res <- findFile' (BS.unpack l)
		return res
		) link

findFile' :: [Octet] -> IO (Either [[Octet]] BS.ByteString)
findFile' cs = do
	let chs = splitFor hashSize cs
	chs' <- findChunks chs
	either (return . Left)
		(\c -> if (length chs) == (hashesPerChunk + 1) then do
			f <- findFile' (BS.unpack (last c))
			either (return . Left) (\ff -> return (Right (BS.concat [(BS.concat (init c)), ff])) ) f
			else
			return (Right (BS.concat c))
		) chs'

-- | tries to locate chunks, returns either list of unavailable ones or list of chunks' content
findChunks :: [[Octet]] -> IO (Either [[Octet]] [BS.ByteString])
findChunks chs = do
	res <- mapM (getChunk) chs
	let unavailable = map (fst) (filter ((== Nothing) . snd) (zip chs res))
	if null unavailable then
		return (Right ((map (unjust) res)))
		else
		return (Left (map (fst) (zip chs (filter (== Nothing) res))))

findChunk :: [Octet] -> IO (Maybe BS.ByteString)
findChunk hsh = do
	fC <- findChunks [hsh]
	either (const (return Nothing)) (return . Just . head) fC