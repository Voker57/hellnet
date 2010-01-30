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

module Hellnet.Network (
	addNode
	, fetchChunk
	, fetchChunks
	, fetchFile
	, fetchMeta
	, fetchMetaFromNode
	, fetchNodeListFromNode
	, findChunk
	, findChunks
	, findFile
	, findMetaValue
	, findURI
	, getContactLog
	, getNodesList
	, handshakeWithNode
	, queryNodeGet
	, updateNodeContactTime
	, writeContactLog
	, writeNodesList
	) where

import Codec.Utils
import Control.Concurrent
import Control.Monad
import qualified Control.Exception as Ex
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BUL
import Data.List
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Debug.Trace
import Hellnet
import Hellnet.Crypto
import Hellnet.Meta as Meta
import Hellnet.Storage
import Hellnet.URI
import Hellnet.Utils
import Network.HTTP
import Network.HTTP.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy.UTF8 as BUL
import Random
import Safe
import System.IO.Error
import Text.HJson as Json
import Text.JSON.JPath

getNodesList :: IO [Node]
getNodesList = do
	listfile <- getFile =<< toFullPath "nodelist"
	let nodes = maybe (Nothing) (readMay . BSL8.unpack) listfile
	return $ maybe [] (id) nodes

writeNodesList :: [Node] -> IO ()
writeNodesList ns = do
	storeFile "nodelist" $ BSL8.pack $ show ns

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
			chID <- insertChunk Nothing r
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
		res <- findFile' key l
		return res
		) link

findFile' :: Maybe Key -> Chunk -> IO (Either [Hash] BSL.ByteString)
findFile' key cs = do
	let chs = splitBslFor hashSize cs
	chs' <- findChunks key $ map (BSL.unpack) chs
	either (return . Left)
		(\c -> if (length chs) == (hashesPerChunk + 1) then do
			f <- findFile' key (last c)
			either (return . Left) (\ff -> return $ Right $ BSL.concat [BSL.concat $ init c, ff]) f
			else
			return $ Right $ BSL.concat c
		) chs'

-- | tries to locate chunks, returns either list of unavailable ones or list of chunks' content
findChunks :: Maybe Key -> [Hash] -> IO (Either [Hash] [Chunk])
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

findChunk :: Maybe Key -> Hash -> IO (Maybe Chunk)
findChunk key hsh = do
	fC <- findChunks key [hsh]
	either (const (return Nothing)) (return . Just . head) fC

-- | prepares file for collecting from storage, returns either list of unavailable chunks or unrolled filelink
fetchFile :: Maybe Key -> Hash -> IO (Either [Hash] [Hash])
fetchFile encKey hsh = do
	link <- findChunk encKey hsh
	maybe (return (Left [hsh])) (\l -> do
		res <- fetchFile' encKey l
		return res
		) link

fetchFile' :: Maybe Key -> Chunk -> IO (Either [Hash] [Hash])
fetchFile' encKey cs = do
	let chs = splitFor hashSize $ BSL.unpack cs
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
			f <- fetchFile' encKey $ unjust c
			return (either (Left) (Right . ((++) (init chs))) f)
			else
			return (Right chs)

queryNodeGet :: String -> Node -> IO (Maybe BSL.ByteString)
queryNodeGet s node = do
	rep <- ((return . Just) =<< simpleHTTP (mkRequest GET $ mkUrl node s)) `catch` const (return Nothing)
	print rep
	return $ maybe Nothing (either (const Nothing) (\rsp -> if rspCode rsp == (2,0,0) then Just (rspBody rsp) else Nothing)) rep

queryNodePost :: String -> [(String, String)] -> Node -> IO (Maybe BSL.ByteString)
queryNodePost s fields node = do
	let dat = BSL8.pack $ urlEncodeVars fields
	let url = mkUrl node s
	let request = (mkRequest POST url :: Request BSL.ByteString) {rqBody=dat, rqHeaders=[Header HdrContentType "application/x-www-form-urlencoded",
		Header HdrContentLength $ show $ BSL8.length dat]}
	rep <- ((return . Just) =<< simpleHTTP request) `catch` const (return Nothing)
	return $ maybe Nothing (either (const Nothing) (\rsp -> if rspCode rsp == (2,0,0) then Just (rspBody rsp) else Nothing)) rep

handshakeWithNode :: Node -> IO Bool
handshakeWithNode node = do
	sP <- getFile "serverport"
	result <- maybe (return False) (\serverPort -> do
		res <- queryNodePost "handshake" [("port", BSL8.unpack serverPort)] node
		return $ maybe (False) (\r -> (BSL8.unpack r) `elem` ["OK","EXISTS"]) res
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
	let res = maybe (Nothing) (readMay . BSL8.unpack) fil
	return $ maybe (Map.fromList []) (id) res

writeContactLog :: (Map String Integer) -> IO ()
writeContactLog l = storeFile "contactlog" (BSL8.pack $ show l)

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
			let str = BUL.toString ch
			either (const (return Nothing)) (\j -> do
				maybe (return Nothing) (\k -> do
					return $ Just $ verifyAsym k msg sig
					) $ fromJson j
				) $ Json.fromString str
			) keyChunk

fetchMeta :: KeyID -> String -> IO Bool
fetchMeta keyId mName = do
	nodes <- getNodesList >>= shuffle
	currentVersion <- getMeta keyId mName
	fetched <- fetchMetaFromNodes nodes currentVersion keyId mName
	case fetched of
		Nothing -> return False
		Just m -> do
			verified <- verifyMeta m
			case verified of
				Just True -> do
					storeMeta m
					return True
				otherwise -> return False

fetchMetaFromNodes :: [Node] -- ^ Nodes' list
	-> Maybe Meta -- ^ Current (latest) version of this meta
	-> KeyID -- ^ Meta's public key ID
	-> String -- ^ Meta name
	-> IO (Maybe Meta)
fetchMetaFromNodes (n:ns) currentVersion k s = do
	fetched <- fetchMetaFromNode n k s
	case fetched of
		Just a -> case currentVersion of
			Nothing -> fetchMetaFromNodes ns Nothing k s
			Just b -> let newerVersion = if timestamp a > timestamp b then a else b
				in fetchMetaFromNodes ns (Just newerVersion) k s
		Nothing -> fetchMetaFromNodes ns currentVersion k s
fetchMetaFromNodes [] cv _ _ = return cv

fetchMetaFromNode :: Node -> KeyID -> String -> IO (Maybe Meta)
fetchMetaFromNode node keyId mName = do
	result <- queryNodeGet (intercalate "/" ["meta", hashToHex keyId, mName]) node
	maybe (return Nothing) (\bs -> do
		let meta = Meta.fromByteString bs
		return meta
		) result


findMetaValue :: KeyID -- ^ public key ID
	-> String -- ^ Meta name
	-> String -- ^ Meta JPath
	-> IO (Maybe [Json]) -- ^ Results or Nothing if meta was not found
findMetaValue keyId mName mPath = do
	meta <- getMeta keyId mName
	case meta of
		Nothing -> return Nothing
		Just m -> do
			conts <- findURI (contentURI m)
			case conts of
				Nothing -> return Nothing
				Just cs -> return $ either (const Nothing) (Just) $ jPath mPath (BUL.toString cs)

fetchNodeListFromNode :: Node -> IO [Node]
fetchNodeListFromNode node = do
	results <- queryNodeGet "/nodelist" node
	return (fromMaybe [] (maybe Nothing (readMay . BSL8.unpack) results))

findURI :: HellnetURI -> IO (Maybe BSL.ByteString)
findURI uri = case uri of
	(ChunkURI hsh key fname) -> do
		findChunk key hsh
	(FileURI hsh key fname) -> do
		fil <- findFile key hsh
		return $ either (const Nothing) (Just) fil
	otherwise -> return Nothing