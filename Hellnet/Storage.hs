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

{-# LANGUAGE FlexibleInstances #-}

module Hellnet.Storage (
	Hellnet.Storage.generateKeyPair
	, deleteMeta
	, getChunk
	, getDirectory
	, getDirectory'
	, getExternalChunk
	, getFile
	, getFile'
	, getIndex
	, getKeyAliases
	, getMeta
	, getMetaNames
	, getPrivateKey
	, hashAndAppend
	, hashToPath
	, indexChunk
	, insertChunk
	, insertData
	, insertFileContents
	, insertIndex
	, isStored
	, purgeChunk
	, regenMeta
	, resolveKeyName
	, storeFile
	, storeFile'
	, storeKeyAliases
	, storeMeta
	, storePrivateKey
	, toFullPath
	) where

import Codec.Crypto.RSA as RSA
import Codec.Utils
import Control.Monad
import Data.Digest.SHA512 as SHA512
import Data.Foldable hiding (concat)
import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Crypto
import Hellnet.ExternalChunks
import Hellnet.Meta as Meta
import Hellnet.URI
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Random
import Text.JSON.JPath
import Text.HJson as JSON

hashAndAppend :: Maybe Key -> Chunk -> Chunk -> IO Chunk
hashAndAppend encKey a b
	| BSL.null b = return a
	| otherwise = do
		bHash <- insertChunk encKey b
		return (BSL.append a $ BSL.pack bHash)

-- | Inserts a file into storage
insertFileContents :: Maybe Key -> BSL.ByteString -> IO Hash
insertFileContents encKey bs = do
	let chunks = splitBslFor chunkSize bs
	chunkHashes <- mapM (insertChunk encKey) chunks
	let fileLink = splitFor hashesPerChunk chunkHashes
	let fileLinkChunks = Prelude.map (BSL.pack . concat) fileLink
	fileLinkHead <- foldrM (hashAndAppend encKey) BSL.empty (fileLinkChunks)
	fileLinkHash <- insertChunk encKey fileLinkHead
	return fileLinkHash

-- | Inserts a chunk into storage. Chunk must be chunkSize or less.
insertChunk :: Maybe Key -> Chunk -> IO Hash
insertChunk encKey ch = do
	let chunk = maybe (ch) ((flip encryptSym) ch) encKey
	let chunkDigestRaw = Hellnet.Crypto.hash chunk
	let chunkDigest = hashToHex chunkDigestRaw
	let fullPath = joinPath ["store", (Prelude.take 2 chunkDigest), (Prelude.drop 2 chunkDigest)]
	storeFile fullPath (chunk)
	return chunkDigestRaw

-- | Fetches chunk from store or index
getChunk :: Maybe Key -> Hash -> IO (Maybe Chunk)
getChunk key hsh = do
	let chunkKey = hashToHex hsh
	chunkLocal <- getFile' ["store", (Prelude.take 2 chunkKey), (Prelude.drop 2 chunkKey)]
	chunk <- case chunkLocal of
		Just fil -> return $ Just fil
		Nothing -> do
			locM <- getIndex hsh
			case locM of
				Nothing -> return Nothing
				Just loc -> getExternalChunk loc
	return $ fmap (\conts -> maybe (conts) (\k -> decryptSym k conts) key) chunk

-- | Converts storage path to full system path
toFullPath :: FilePath -> IO FilePath
toFullPath fpath = do
	dirEnv <- safeGetEnv "HELLNET_HOME"
	dir <- maybe (getAppUserDataDirectory "hellnet") (return) dirEnv
	return (joinPath [dir,fpath])

-- | Stores file using given path in storage
storeFile :: FilePath -> BSL.ByteString -> IO ()
storeFile fpath dat = do
	fullPath <- toFullPath fpath
	createDirectoryIfMissing True (dropFileName fullPath)
	(tmpf, tmph) <- openTempFile "/tmp" "helltemp"
	BSL.hPut tmph dat
	hFlush tmph
	hClose tmph
	let copyMove = do
		copyFile tmpf fullPath
		removeFile tmpf
	catch (renameFile tmpf fullPath) (\e -> copyMove)

-- | Convenience version of storeFile that joins path by itself
storeFile' :: [String] -> BSL.ByteString -> IO ()
storeFile' fs = storeFile (joinPath fs)

-- | Gets file from storage path
getFile :: FilePath -> IO (Maybe BSL.ByteString)
getFile fpath = do
	fullPath <- toFullPath fpath
	catch (do
		conts <- BSL.readFile fullPath
		return (Just conts)) (const (return Nothing))

-- | See storeFile'
getFile' :: [String] -> IO (Maybe BSL.ByteString)
getFile' fs = getFile (joinPath fs)

-- | Removes chunk from storage
purgeChunk :: Hash -> IO ()
purgeChunk hsh = do
	let hexHsh = hashToHex hsh
	fpath <- toFullPath (joinPath ["store", (Prelude.take 2 hexHsh), (Prelude.drop 2 hexHsh)])
	removeFile fpath

-- | Converts hash to chunk path (first byte as directory)
hashToPath :: Hash -> IO FilePath
hashToPath hsh = let hexHsh = hashToHex hsh in toFullPath (joinPath ["store", (Prelude.take 2 hexHsh), (Prelude.drop 2 hexHsh)])

-- | Whether chunk with that hash is stored already
isStored :: Hash -> IO Bool
isStored hsh = do
	c <- getChunk Nothing hsh
	return $ isJust c

-- | Gets meta from storage
getMeta :: KeyID -> String -> IO (Maybe Meta)
getMeta keyId mName = do
	mFile <- getFile' ["meta", hashToHex keyId, mName]
	return $ maybe (Nothing) (Meta.fromByteString) mFile

-- | Gets all meta names for given key
getMetaNames :: KeyID -> IO [String]
getMetaNames keyid = do
	let path = ["meta", hashToHex keyid]
	res <- getDirectory' path
	return $ fromMaybe [] res

-- | Stores meta in storage
storeMeta :: Meta -> IO ()
storeMeta m = storeFile' ["meta", hashToHex (keyID m), metaName m] $ Meta.toByteString m

-- | Returns list of all directory entries
getDirectory :: FilePath -> IO (Maybe [FilePath])
getDirectory fpath = do
	fullPath <- toFullPath fpath
	exists <- doesDirectoryExist fullPath
	if exists then do
		conts <- getDirectoryContents fullPath
		return $ Just $ conts \\ [".", ".."]
		else
		return Nothing

-- | See storeFile'
getDirectory' :: [FilePath] -> IO (Maybe [FilePath])
getDirectory' = getDirectory . joinPath

-- | Inserts data in optimal form (chunk or file), returns URI
insertData :: Maybe Key -> BSL.ByteString -> IO HellnetURI
insertData encKey dat = if BSL.null $ BSL.drop (256 * 1024) dat then do
	hsh <- insertChunk encKey dat
	return $ ChunkURI hsh encKey Nothing
	else do
	hsh <- insertFileContents encKey dat
	return $ FileURI hsh encKey Nothing

-- | Stores private ky in storage
storePrivateKey :: KeyID -> PrivateKey -> IO ()
storePrivateKey kid pKey = do
	let bs = BUL.fromString $ JSON.toString $ toJson pKey
	storeFile' ["privatekeys", hashToHex kid] bs
	fPath <- toFullPath $ joinPath ["privatekeys", hashToHex kid]
	setFileMode fPath $ unionFileModes ownerWriteMode ownerReadMode

-- | Generates key pair and stores'em
generateKeyPair :: IO KeyID
generateKeyPair = do
	g <- newStdGen
	let (pub, priv, _) = RSA.generateKeyPair g 1024
	hsh <- insertChunk Nothing $ BUL.fromString $ JSON.toString $ toJson pub
	storePrivateKey hsh priv
	return hsh

-- | Gets private key from storage
getPrivateKey :: KeyID -> IO (Maybe PrivateKey)
getPrivateKey kid = do
	fil <- getFile' ["privatekeys", hashToHex kid]
	return $ case fil of
		Nothing -> Nothing
		Just k -> either (const Nothing) (fromJson) $ JSON.fromString $ BUL.toString k

-- | Regenerates meta message and signature if private key is available
regenMeta :: Meta -> IO (Maybe Meta)
regenMeta meta = do
	pKeyM <- getPrivateKey (keyID meta)
	case pKeyM of
		Nothing -> return Nothing
		Just pKey -> do
			updatedV <- getUnixTime
			let sigV = BUL.fromString "FOOBAR"
			let messageV = BUL.fromString $ toString $ toJson $ meta {timestamp = updatedV}
			let sigV = signAsym pKey messageV
			return $ Just $ meta {timestamp = updatedV, message = Just messageV, signature = Just sigV}

instance Jsonable (Map.Map String KeyID) where
	toJson m = JObject $ Map.map (stringifyValue) m where
		stringifyValue v = JString $ hashToHex v
	fromJson (JObject m) = Just $ Map.map (hexToHash . unStringifyValue) m where
		unStringifyValue (JString v) = v
		unStringifyValue _ = ""
	fromJson _ = Nothing

-- | Returns all alias
getKeyAliases :: IO (Map.Map String KeyID)
getKeyAliases = do
	aliasesFileM <- getFile "keyaliases"
	return $ case aliasesFileM of
		Nothing -> Map.empty
		Just aliasesFile -> case JSON.fromString $ BUL.toString aliasesFile of
			Left _ -> Map.empty
			Right j -> fromMaybe (Map.empty) $ fromJson j

-- | Stores alias' map
storeKeyAliases :: Map.Map String KeyID -> IO ()
storeKeyAliases m = storeFile "keyaliases" $ BUL.fromString $ JSON.toString $ toJson m

-- | Resolves an alias
resolveKeyName :: String -> IO KeyID
resolveKeyName name = do
	aliases <- getKeyAliases
	return $ fromMaybe (hexToHash name) $ Map.lookup name aliases

-- | Return chunk hash, as it would be in database (dry run of insertChunk)
hashChunk :: Maybe Key -> Chunk -> IO Hash
hashChunk encKey ch = do
	let chunk = maybe (id) (encryptSym) encKey $ ch
	let chunkDigestRaw = Hellnet.Crypto.hash chunk
	return chunkDigestRaw

-- | Indexes chunk
indexChunk :: Maybe Key -> Chunk -> ChunkLocation -> IO Hash
indexChunk encKey ch cl = do
	hsh <- hashChunk encKey ch
	insertIndex hsh cl
	return hsh

-- | Inserts index into map
insertIndex :: Hash -> ChunkLocation -> IO ()
insertIndex hsh cl = do
	storeFile' ["chunkmap", hashToHex $ take 1 hsh, hashToHex $ drop 1 hsh] $ BUL.fromString $ JSON.toString $ toJson cl

-- | Looks up index in map
getIndex :: Hash -> IO (Maybe ChunkLocation)
getIndex hsh = do
	contentM <- getFile' ["chunkmap", hashToHex $ take 1 hsh, hashToHex $ drop 1 hsh]
	return $ case contentM of
		Nothing -> Nothing
		Just content -> case JSON.fromString $ BUL.toString content of
			Left _ -> Nothing
			Right js -> fromJson js

-- | Gets chunk from external location
getExternalChunk :: ChunkLocation -> IO (Maybe Chunk)
getExternalChunk (FileLocation path offset encKey) = do
	exists <- doesFileExist path
	if exists then do
		fH <- openFile path ReadMode
		hSeek fH AbsoluteSeek offset
		chunk <- BSL.hGet fH $ fromInteger chunkSize
		return $ Just $ maybe (id) (encryptSym) encKey $ chunk
		else
		return Nothing

-- | Deletes meta
deleteMeta :: KeyID -> String -> IO Bool
deleteMeta kid mname = do
	fpath <- toFullPath $ joinPath ["meta", hashToHex kid, mname]
	exists <- doesFileExist fpath
	if exists then do
		removeFile fpath
		return True
		else
		return False