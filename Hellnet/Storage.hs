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

module Hellnet.Storage (insertFileContents, insertFileContentsLazy, getChunk, insertChunk, toFullPath, purgeChunk, storeFile, storeFile', getFile, getFile', hashToPath, isStored) where

import Codec.Utils
import Control.Monad
import Data.Digest.SHA512 as SHA512
import Data.Foldable hiding (concat)
import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Crypto
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
-- import OpenSSL.DSA
import System.Directory
import System.FilePath
import System.Posix.Files

hashAndAppend :: Maybe Key -> Chunk -> Chunk -> IO Hash
hashAndAppend _ a [] = do return a
hashAndAppend encKey a b = do
	bChunk <- insertChunk encKey b
	return (a ++ bChunk)

insertFileContentsLazy :: Maybe Key -> BSL.ByteString -> IO Hash
insertFileContentsLazy encKey bs = do
	let chunks = splitBslFor chunkSize bs
	chunkHashes <- mapM ((insertChunk encKey) . BSL.unpack) chunks
	let fileLink = splitFor hashesPerChunk chunkHashes
	let fileLinkChunks = Prelude.map (flatten) fileLink
		where flatten a = Prelude.foldl1 (++) a
	fileLinkHead <- foldrM (hashAndAppend encKey) [] (fileLinkChunks)
	fileLinkHash <- insertChunk encKey fileLinkHead
	return fileLinkHash

insertFileContents :: Maybe Key -> BS.ByteString -> IO Hash
insertFileContents k b = insertFileContentsLazy k (BSL.pack $ BS.unpack b)

insertChunkBS :: Maybe Key -> BS.ByteString -> IO Hash
insertChunkBS k c = insertChunk k (BS.unpack c)

insertChunk :: Maybe Key -> Chunk -> IO Hash
insertChunk encKey ch
	| length ch <= chunkSize = do
		let chunk = maybe (ch) ((flip encryptAES) ch) encKey
		let chunkDigestRaw = SHA512.hash chunk
		let chunkDigest = hashToHex chunkDigestRaw
		let fullPath = joinPath ["store", (Prelude.take 2 chunkDigest), (Prelude.drop 2 chunkDigest)]
		storeFile fullPath (BS.pack chunk)
		return chunkDigestRaw

getChunk :: Maybe Key -> Hash -> IO (Maybe BS.ByteString)
getChunk key hsh = do
	let chunkKey = hashToHex hsh
	filepath <- toFullPath (joinPath ["store", (Prelude.take 2 chunkKey), (Prelude.drop 2 chunkKey)])
	fil <- getFile filepath
	return $ maybe Nothing (\conts -> Just (maybe (conts) (\k -> BS.pack $ decryptAES k $ BS.unpack conts) key)) fil

toFullPath :: FilePath -> IO FilePath
toFullPath fpath = do
	dir <- getAppUserDataDirectory "hellnet"
	return (joinPath [dir,fpath])

storeFile :: FilePath -> BS.ByteString -> IO ()
storeFile fpath dat = do
	fullPath <- toFullPath fpath
	createDirectoryIfMissing True (dropFileName fullPath)
	BS.writeFile fullPath dat

storeFile' :: [String] -> BS.ByteString -> IO ()
storeFile' fs = storeFile (joinPath fs)

getFile :: FilePath -> IO (Maybe BS.ByteString)
getFile fpath = do
	fullPath <- toFullPath fpath
	catch (do
		conts <- BS.readFile fullPath
		return (Just conts)) (const (return Nothing))

getFile' :: [String] -> IO (Maybe BS.ByteString)
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