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

module Hellnet.Storage (insertFileContents, getFileContents, getChunk, insertChunk, toFullPath, purgeChunk) where

import Data.ByteString as BS
import Data.Digest.SHA512 as SHA512
import Codec.Utils
import Data.Maybe
import Control.Monad
import System.Directory
import System.FilePath
import Hellnet
import Hellnet.Utils
import Data.Foldable

hashAndAppend :: [Octet] -> [Octet] -> IO [Octet]
a `hashAndAppend` [] = do return a
a `hashAndAppend` b = do
	bChunk <- insertChunk (BS.pack b)
	return (a ++ bChunk)

insertFileContents :: ByteString -> IO [Octet]
insertFileContents bs = do
	let chunks = splitBsFor chunkSize bs
	Prelude.putStrLn "inserting chunks"
	chunkHashes <- mapM (insertChunk) chunks
	let fileLink = splitFor hashesPerChunk chunkHashes
	let fileLinkChunks = Prelude.map (flatten) fileLink
		where flatten a = Prelude.foldl1 (++) a
	fileLinkHead <- foldlM (hashAndAppend) [] (fileLinkChunks)
	fileLinkHash <- insertChunk (BS.pack fileLinkHead)
	return fileLinkHash

insertChunk :: ByteString -> IO [Octet]
insertChunk chunk
	| BS.length chunk <= chunkSize = do
		let chunkDigestRaw = SHA512.hash (BS.unpack chunk)
		let chunkDigest = hashToHex chunkDigestRaw
		let fullPath = joinPath ["store", (Prelude.take 2 chunkDigest), (Prelude.drop 2 chunkDigest)]
		storeFile fullPath (chunk)
		return chunkDigestRaw

getChunk :: [Octet] -> IO ByteString
getChunk hsh = do
	let chunkKey = hashToHex hsh
	chunk <- getFile (joinPath ["store", (Prelude.take 2 chunkKey), (Prelude.drop 2 chunkKey)])
	return chunk

getFileContents :: [Octet] -> IO ByteString
getFileContents hsh = do
	chunk <- getChunk hsh
	let pieces = splitBsFor hashSize chunk
	morechunks <- if (Prelude.length pieces) == 1024 then
		getFileContents (BS.unpack $ (Prelude.last pieces))
		else return BS.empty
	let morechunks' = splitBsFor chunkSize morechunks
	conts <- mapM (getChunk . BS.unpack) (pieces ++ morechunks')
	return (BS.concat conts)

toFullPath :: FilePath -> IO FilePath
toFullPath fpath = do
	dir <- getAppUserDataDirectory "hellnet"
	return (joinPath [dir,fpath])

storeFile :: FilePath -> ByteString -> IO ()
storeFile fpath dat = do
	fullPath <- toFullPath fpath
	createDirectoryIfMissing True (dropFileName fullPath)
	BS.writeFile fullPath dat

getFile :: FilePath -> IO ByteString
getFile fpath = do
	fullPath <- toFullPath fpath
	conts <- BS.readFile fullPath
	return conts

purgeChunk :: [Octet] -> IO ()
purgeChunk hsh = do
	let hexHsh = hashToHex hsh
	fpath <- toFullPath (joinPath ["store", (Prelude.take 2 hexHsh), (Prelude.drop 2 hexHsh)])
	removeFile fpath