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

module Hellnet.Network (fetchChunk, fetchChunks, getNodesList, writeNodesList, findChunk, findChunks, findFile, fetchFile, queryNodeGet, addNode, handshakeWithNode, updateNodeContactTime, getContactLog, writeContactLog, fetchMeta, fetchMetaFromNode) where

import Codec.Utils
import Control.Concurrent
import Control.Monad
import qualified Control.Exception as Ex
import qualified Data.ByteString.UTF8 as BU
import Data.List
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Debug.Trace
import Hellnet
import Hellnet.Crypto
import Hellnet.Meta as Meta
import Hellnet.Storage
import Hellnet.Utils
import Network.HTTP
import Network.HTTP.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Random
import Safe
import System.IO.Error
import Text.HJson as Json
import Text.JSON.JPath

getNodesList :: IO [Node]
getNodesList = do
	listfile <- getFile =<< toFullPath "nodelist"
	let nodes = maybe (Nothing) (readMay . BS8.unpack) listfile
	return $ maybe [] (id) nodes

writeNodesList :: [Node] -> IO ()
writeNodesList ns = do
	storeFile "nodelist" $ BS8.pack $ show ns

-- | adds node to list if it isn't already there, returns true
-- | otherwise returns false
addNode :: Node -> IO Bool
addNode node = do
	nL <- getNodesList
	if node `elem` nL then return False
		else do
			writeNodesList (node:nL)
			return True

-- | retrieves pieces using servers node list and returns list of pieces that are unavailable.
fetchChunks :: [Hash] -> IO [Hash]
fetchChunks cs = do
	nodes <- getNodesList
	decoys <- mapM (const genHash) [0..(((length cs) `div` 3) + 1)]
	let decoys' = decoys \\ cs
	chunks <- shuffle (cs ++ decoys')
	let todos = splitFor numThreads chunks
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
fetchChunk node p = do
	let chunkID = hashToHex p
	let reqString = "chunks/" ++ (take 2 chunkID) ++ "/" ++ (drop 2 chunkID)
	req <- queryNodeGet reqString node
	maybe (return False)
		(\r -> do
			chID <- insertChunk Nothing $ BS.unpack $ BS8.pack r
			if (chID == p) then
				return True
				else do
					purgeChunk chID
					return False
			) req

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

queryNodeGet :: String -> Node -> IO (Maybe String)
queryNodeGet s node = do
	print $ mkUrl node s
	rep <- ((return . Just) =<< simpleHTTP (getRequest $ mkUrl node s)) `catch` const (return Nothing)
	print rep
	return $ maybe Nothing (either (const Nothing) (\rsp -> if rspCode rsp == (2,0,0) then Just (rspBody rsp) else Nothing)) rep

queryNodePost :: String -> [(String, String)] -> Node -> IO (Maybe String)
queryNodePost s fields node = do
	let dat = urlEncodeVars fields
	let url = mkUrl node s
	let request = (postRequest url) {rqBody=dat, rqHeaders=[Header HdrContentType "application/x-www-form-urlencoded",
		Header HdrContentLength $ show $ length dat]}
	rep <- ((return . Just) =<< simpleHTTP request) `catch` const (return Nothing)
	return $ maybe Nothing (either (const Nothing) (\rsp -> if rspCode rsp == (2,0,0) then Just (rspBody rsp) else Nothing)) rep

handshakeWithNode :: Node -> IO Bool
handshakeWithNode node = do
	sP <- getFile "serverport"
	result <- maybe (return False) (\serverPort -> do
		res <- queryNodePost "handshake" [("port", BS8.unpack serverPort)] node
		return $ maybe (False) ((flip elem) ["OK","EXISTS"]) res
		) sP
	when result (discard =<< addNode node)
	return result

updateNodeContactTime :: String -> Integer -> IO ()
updateNodeContactTime hst tim = do
	tehLog <- getContactLog
	writeContactLog (Map.insert hst tim tehLog)

getContactLog :: IO (Map String Integer)
getContactLog = do
	fil <- getFile "contactlog"
	let res = maybe (Nothing) (readMay . BS8.unpack) fil
	return $ maybe (Map.fromList []) (id) res

writeContactLog :: (Map String Integer) -> IO ()
writeContactLog l = storeFile "contactlog" (BS8.pack $ show l)

-- | Verify meta's signature
-- | Returns Nothing if public key was not found or Just (check result)
verifyMeta :: Meta -> IO (Maybe Bool)
verifyMeta meta
	| isNothing (signature meta) = return $ Just False
	| isNothing (message meta) = return Nothing
	| otherwise = do
		let (Just msg, Just sig) = (message meta, signature meta)
		keyChunk <- findChunk Nothing (keyID meta)
		maybe (return Nothing) (\ch -> do
			let str = BU.toString ch
			either (const (return Nothing)) (\j -> do
				maybe (return Nothing) (\k -> do
					return $ Just $ verifyAsym k (relaxByteString msg) (relaxByteString sig)
					) $ fromJson j
				) $ Json.fromString str
			) keyChunk

fetchMeta :: KeyID -> String -> IO Bool
fetchMeta keyId mName = undefined

fetchMetaFromNode :: Node -> KeyID -> String -> IO (Maybe Meta)
fetchMetaFromNode node keyId mName = do
	result <- queryNodeGet (intercalate "/" ["meta", hashToHex keyId, mName]) node
	maybe (return Nothing) (\s -> do
		-- FIXME: Stringfuck, harmless but
		let bs = BS8.pack s
		let meta = Meta.fromByteString bs
		return meta
		) result