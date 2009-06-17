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

module Hellnet.Storage (insertFileContents, getChunk, insertChunk, toFullPath, purgeChunk, storeFile, hashToPath, isStored, getHashesByMeta, addHashesToMeta) where

import Codec.Utils
import Control.Monad
import Data.Digest.SHA512 as SHA512
import Data.Foldable hiding (concat)
import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import System.FilePath

hashAndAppend :: Maybe Key -> Chunk -> Chunk -> IO Hash
hashAndAppend _ a [] = do return a
hashAndAppend encKey a b = do
	bChunk <- insertChunk encKey b
	return (a ++ bChunk)

insertFileContents :: Maybe Key -> BSL.ByteString -> IO Hash
insertFileContents encKey bs = do
	let chunks = splitBslFor chunkSize bs
	chunkHashes <- mapM ((insertChunk encKey) . BSL.unpack) chunks
	let fileLink = splitFor hashesPerChunk chunkHashes
	let fileLinkChunks = Prelude.map (flatten) fileLink
		where flatten a = Prelude.foldl1 (++) a
	fileLinkHead <- foldrM (hashAndAppend encKey) [] (fileLinkChunks)
	fileLinkHash <- insertChunk encKey fileLinkHead
	return fileLinkHash

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
	exists <- doesFileExist filepath
	res <- (if exists then do
		conts <- BS.readFile filepath
		return (Just (maybe (conts) (\k -> BS.pack $ decryptAES k $ BS.unpack conts) key))
		else
		return Nothing)
	return res

toFullPath :: FilePath -> IO FilePath
toFullPath fpath = do
	dir <- getAppUserDataDirectory "hellnet"
	return (joinPath [dir,fpath])

storeFile :: FilePath -> BS.ByteString -> IO ()
storeFile fpath dat = do
	fullPath <- toFullPath fpath
	createDirectoryIfMissing True (dropFileName fullPath)
	BS.writeFile fullPath dat

getFile :: FilePath -> IO BS.ByteString
getFile fpath = do
	fullPath <- toFullPath fpath
	conts <- BS.readFile fullPath
	return conts

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

getHashesByMeta :: String -> String -> IO [Hash]
getHashesByMeta key value = do
	let [k,v] = map (dropWhile (=='/')) [key,value]
	conts <- catch (getFile $ joinPath ["meta",k,v]) (const $ return BS.empty)
	return $ splitFor hashSize $ BS.unpack conts

addHashesToMeta :: String -> String -> [Hash] -> IO ()
addHashesToMeta value key hs = do
	current <- catch (getFile (joinPath ["meta",value,key])) (const $ return BS.empty)
	let currentList = splitFor hashSize (BS.unpack current)
	storeFile (joinPath ["meta",value,key]) $ BS.pack $ concat $ nub (currentList ++ hs)