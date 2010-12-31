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

{-# LANGUAGE RelaxedPolyRec #-}

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
	, findKey
	, findMeta
	, findMetaContent
	, findMetaContent'
	, findMetaContentByName
	, findMetaContentByName'
	, findURI
	, getContactLog
	, getNodesList
	, handshakeWithNode
	, modifyMetaContent
	, modifyMetaContent'
	, queryNodeGet
	, updateNodeContactTime
	, verifyMeta
	, writeContactLog
	, writeNodesList
	) where

import Codec.Crypto.RSA as RSA
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
import System.Timeout
import Text.HJson as JSON
import Text.JSON.JPath as JPath

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
		(\c -> if (length chs) == (fromInteger hashesPerChunk + 1) then do
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
		if (length chs) == (fromInteger hashesPerChunk + 1) then do
			c <- getChunk encKey $ last chs
			f <- fetchFile' encKey $ unjust c
			return (either (Left) (Right . ((++) (init chs))) f)
			else
			return (Right chs)

queryNodeGet :: String -> Node -> IO (Maybe BSL.ByteString)
queryNodeGet s node = do
	timeoutValue <- getTimeoutValue
	rep <- timeout timeoutValue $ ((return . Just) =<< simpleHTTP (mkRequest GET $ mkUrl node s)) `catch` const (return Nothing)
	return $ maybe Nothing (either (const Nothing) (\rsp -> if rspCode rsp == (2,0,0) then Just (rspBody rsp) else Nothing)) $ join rep

queryNodePost :: String -> [(String, String)] -> Node -> IO (Maybe BSL.ByteString)
queryNodePost s fields node = do
	let dat = BSL8.pack $ urlEncodeVars fields
	let url = mkUrl node s
	let request = (mkRequest POST url :: Request BSL.ByteString) {rqBody=dat, rqHeaders=[Header HdrContentType "application/x-www-form-urlencoded",
		Header HdrContentLength $ show $ BSL8.length dat]}
	timeoutValue <- getTimeoutValue
	rep <- timeout timeoutValue $ ((return . Just) =<< simpleHTTP request) `catch` const (return Nothing)
	return $ maybe Nothing (either (const Nothing) (\rsp -> if rspCode rsp == (2,0,0) then Just (rspBody rsp) else Nothing)) $ join rep

-- | Perform handshake with node. Return Left (error message) if connection failed or Right (handshake success)
handshakeWithNode :: Node -> IO (Either String Bool)
handshakeWithNode node = do
	sP <- getFile "serverport"
	result <- maybe (return $ Left "Server not running?") (\serverPort -> do
		res <- queryNodePost "handshake" [("port", BSL8.unpack serverPort)] node
		return $ maybe (Left "Failed to contact node") (\r -> Right $ (BSL8.unpack r) `elem` ["OK","EXISTS"]) res
		) sP
	return result

-- | Get HTTP timeout value, either default or from environment
getTimeoutValue :: IO Int
getTimeoutValue = do
	t <- safeGetEnv "HELLNET_TIMEOUT"
	return $ fromMaybe 5000000 $ join $ fmap (readMay) t

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
				) $ JSON.fromString str
			) keyChunk

-- | Fetches meta from network into storage, return success
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
			Nothing -> fetchMetaFromNodes ns fetched k s
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

-- | Locates information by given JPath in given meta's content
-- | Works only on meta that contains JSON
findMetaContent :: (JPath.QueryLike a) => Maybe Key
	-> Meta
	-> a -- ^ Meta JPath
	-> IO (Maybe [Json]) -- ^ Results or Nothing if meta was not found
findMetaContent metaKey meta mPath = do
	contBS <- findMetaContent' metaKey meta
	case contBS of
		Nothing -> return Nothing
		Just cont -> return $ case JSON.fromString $ BUL.toString cont of
			Left _ -> Nothing
			Right js -> Just $ jPath mPath js

-- | Locates given meta content
findMetaContent' :: Maybe Key -> Meta -> IO (Maybe BSL.ByteString)
findMetaContent' metaKey m = do
	cont <- findURI (contentURI m)
	return $ fmap (maybe (id) (decryptSym) metaKey) cont

-- | Same as findMetaContent, byt grabs meta from storage first
findMetaContentByName :: (QueryLike a) => Maybe Key -> KeyID -> String -> a -> IO (Maybe [Json])
findMetaContentByName metaKey kid mname mpath = do
	metaM <- getMeta kid mname
	case metaM of
		Nothing -> return Nothing
		Just meta -> findMetaContent metaKey meta mpath

-- | Same as findMetaContent', byt grabs meta from storage first
findMetaContentByName' :: Maybe Key -> KeyID -> String -> IO (Maybe BSL.ByteString)
findMetaContentByName' metaKey kid mname = do
	metaM <- getMeta kid mname
	case metaM of
		Nothing -> return Nothing
		Just meta -> findMetaContent' metaKey meta

-- |  Modifies meta content using given IO action and tries to re-sign it
modifyMetaContent' :: Maybe Key -- ^ Meta contents' key
	-> Meta
	-> Maybe Key -- ^ Meta key
	-> (BUL.ByteString -> IO (Maybe BUL.ByteString))
	-> IO Bool -- ^ Whether meta was successfully modified
modifyMetaContent' encKey meta metaKey modifier = do
	contM <- findMetaContent' encKey meta
	case contM of
		Nothing -> return False
		Just cont -> do
			modifiedContentM <- modifier cont
			case modifiedContentM of
				Just modifiedContent -> do
					uri <- insertData encKey (maybe (id) (encryptSym) metaKey modifiedContent)
					newmetaM <- regenMeta $ meta {contentURI = uri}
					case newmetaM of
						Nothing -> return False
						Just newmeta -> do
							storeMeta newmeta
							return True
				Nothing -> return False

-- |  Modifies meta content under JPath using given IO action and tries to re-sign it
-- | Works only on JSON-containing meta
modifyMetaContent :: (JPath.QueryLike a) => Maybe Key -- ^ Meta contents' key
	-> Meta
	-> a -- ^ Meta JPath
	-> Maybe Key -- ^ Meta key
	-> (Json -> IO (Maybe Json))
	-> IO Bool -- ^ Whether meta was successfully modified
modifyMetaContent encKey meta mpath metaKey modifier = do
	let bsModifier bs = case JSON.fromString $ BUL.toString bs of
		Left err -> return Nothing
		Right js -> do
			case jPath mpath js of
				[jsPart] -> do
					jsPart' <- modifier jsPart
					return $ fmap (\p -> BUL.fromString $ JSON.toString $ jPathModify mpath (const p) js) jsPart'
				otherwise -> return Nothing
	modifyMetaContent' encKey meta metaKey bsModifier

fetchNodeListFromNode :: Node -> IO [Node]
fetchNodeListFromNode node = do
	results <- queryNodeGet "/nodelist" node
	return (fromMaybe [] (maybe Nothing (readMay . BUL.toString) results))

-- | Tries to download given URI
findURI :: HellnetURI -> IO (Maybe BSL.ByteString)
findURI uri = case uri of
	(ChunkURI hsh key _) -> do
		findChunk key hsh
	(FileURI hsh key _) -> do
		fil <- findFile key hsh
		return $ either (const Nothing) (Just) fil
	(MetaURI hsh (Just mName) [] mKey _) -> do
		-- Fetch raw meta data
		findMetaContentByName' mKey hsh mName
	(MetaURI hsh (Just mName) mPath mKey _) -> do
		-- Fetch raw meta data
		findMetaContentByName mKey hsh mName mPath >>= return . fmap (BUL.fromString . JSON.toString . JSON.JArray)
	u@(CryptURI _) -> case decryptURI u of
		Just (CryptURI _) -> return Nothing
		Just dy -> findURI dy
	otherwise -> return Nothing

-- | Tries to find public key in network by its ID
findKey :: KeyID -> IO (Maybe PublicKey)
findKey kid = do
	ch <- findChunk Nothing kid
	return $ case ch of
		Nothing -> Nothing
		Just chunk -> either (const Nothing) (fromJson) $ JSON.fromString $ BUL.toString $ chunk

-- | Tries to find meta in network
findMeta :: KeyID -> String -> IO (Maybe Meta)
findMeta kid name = do
	fetchMeta kid name
	getMeta kid name