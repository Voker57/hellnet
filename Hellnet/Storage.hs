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

module Hellnet.Storage (
	getChunk
	, getFile
	, getFile'
	, getMeta
	, hashToPath
	, insertChunk
	, insertFileContents
	, insertFileContentsLazy
	, isStored
	, purgeChunk
	, storeFile
	, storeFile'
	, storeMeta
	, toFullPath
	) where

import Codec.Utils
import Control.Monad
import Data.Digest.SHA512 as SHA512
import Data.Foldable hiding (concat)
import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Crypto
import Hellnet.Meta as Meta
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
-- import OpenSSL.DSA
import System.Directory
import System.FilePath
import System.Posix.Files
import Text.JSON.JPath
import Text.HJson

hashAndAppend :: Maybe Key -> Chunk -> Chunk -> IO Chunk
hashAndAppend encKey a b
	| BSL.null b = return a
	| otherwise = do
		bHash <- insertChunk encKey b
		return (BSL.append a $ BSL.pack bHash)

insertFileContentsLazy :: Maybe Key -> BSL.ByteString -> IO Hash
insertFileContentsLazy encKey bs = do
	let chunks = splitBslFor chunkSize bs
	chunkHashes <- mapM (insertChunk encKey) chunks
	let fileLink = splitFor hashesPerChunk chunkHashes
	let fileLinkChunks = Prelude.map (BSL.pack . flatten) fileLink
		where flatten a = Prelude.foldl1 (++) a
	fileLinkHead <- foldrM (hashAndAppend encKey) BSL.empty (fileLinkChunks)
	fileLinkHash <- insertChunk encKey fileLinkHead
	return fileLinkHash

insertFileContents :: Maybe Key -> BS.ByteString -> IO Hash
insertFileContents k b = insertFileContentsLazy k (BSL.pack $ BS.unpack b)

insertChunk :: Maybe Key -> Chunk -> IO Hash
insertChunk encKey ch = do
	let chunk = maybe (ch) ((flip encryptSym) ch) encKey
	let chunkDigestRaw = Hellnet.Crypto.hash chunk
	let chunkDigest = hashToHex chunkDigestRaw
	let fullPath = joinPath ["store", (Prelude.take 2 chunkDigest), (Prelude.drop 2 chunkDigest)]
	storeFile fullPath (chunk)
	return chunkDigestRaw

getChunk :: Maybe Key -> Hash -> IO (Maybe Chunk)
getChunk key hsh = do
	let chunkKey = hashToHex hsh
	filepath <- toFullPath (joinPath ["store", (Prelude.take 2 chunkKey), (Prelude.drop 2 chunkKey)])
	fil <- getFile filepath
	return $ maybe Nothing (\conts -> Just (maybe (conts) (\k -> decryptSym k conts) key)) fil

toFullPath :: FilePath -> IO FilePath
toFullPath fpath = do
	dir <- getAppUserDataDirectory "hellnet"
	return (joinPath [dir,fpath])

storeFile :: FilePath -> BSL.ByteString -> IO ()
storeFile fpath dat = do
	fullPath <- toFullPath fpath
	createDirectoryIfMissing True (dropFileName fullPath)
	BSL.writeFile fullPath dat

storeFile' :: [String] -> BSL.ByteString -> IO ()
storeFile' fs = storeFile (joinPath fs)

getFile :: FilePath -> IO (Maybe BSL.ByteString)
getFile fpath = do
	fullPath <- toFullPath fpath
	catch (do
		conts <- BSL.readFile fullPath
		return (Just conts)) (const (return Nothing))

getFile' :: [String] -> IO (Maybe BSL.ByteString)
getFile' fs = getFile (joinPath fs)

purgeChunk :: Hash -> IO ()
purgeChunk hsh = do
	let hexHsh = hashToHex hsh
	fpath <- toFullPath (joinPath ["store", (Prelude.take 2 hexHsh), (Prelude.drop 2 hexHsh)])
	removeFile fpath

hashToPath :: Hash -> IO FilePath
hashToPath hsh = let hexHsh = hashToHex hsh in toFullPath (joinPath ["store", (Prelude.take 2 hexHsh), (Prelude.drop 2 hexHsh)])

isStored :: Hash -> IO Bool
isStored hsh = do
	fp <- hashToPath hsh
	return =<< (doesFileExist fp)

getMeta :: KeyID -> String -> IO (Maybe Meta)
getMeta keyId mName = do
	mFile <- getFile' ["meta", hashToHex keyId, mName]
	return $ maybe (Nothing) (Meta.fromByteString) mFile

getMetaValue :: KeyID -- ^ public key ID
	-> String -- ^ Meta name
	-> String -- ^ Meta JPath
	-> IO (Maybe [Json]) -- ^ Results or Nothing if meta was not found
getMetaValue keyId mName mPath = do
	meta <- getMeta keyId mName
	return $ maybe ( Nothing) (\m ->
		either (const Nothing) (Just) $ jPath mPath (BUL.toString $ fromMaybe BSL.empty $ message m)
		) meta

modifyMeta :: KeyID -- ^ public key ID
	-> String -- ^ Meta name
	-> String -- ^ Meta JPath
	-> (Json -> Json) -- ^ JSON modifier function
	-> Bool -- Whether meta was successfully modified
modifyMeta = undefined

storeMeta :: Meta -> IO ()
storeMeta m = storeFile' ["meta", hashToHex (keyID m), metaName m] $ Meta.toByteString m