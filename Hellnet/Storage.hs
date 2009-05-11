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
	bChunk <- insertChunk b
	return (a ++ b)

insertFileContents :: [Octet] -> IO [Octet]
insertFileContents bs = do
	let chunks = splitFor chunkSize bs
	chunkHashes <- mapM (insertChunk) chunks
	let fileLink = splitFor hashesPerChunk chunkHashes
	let fileLinkChunks = Prelude.map (flatten) fileLink
		where flatten a = Prelude.foldl1 (++) a
	fileLinkHead <- foldlM (hashAndAppend) [] (fileLinkChunks)
	fileLinkHash <- insertChunk fileLinkHead
	return fileLinkHash

insertChunk :: [Octet] -> IO [Octet]
insertChunk chunk
	| Prelude.length chunk <= chunkSize = do
		let chunkDigestRaw = SHA512.hash chunk
		let chunkDigest = hashToHex chunkDigestRaw
		let fullPath = joinPath ["store", (Prelude.take 2 chunkDigest), (Prelude.drop 2 chunkDigest)]
		storeFile fullPath (BS.pack chunk)
		return chunkDigestRaw

getChunk :: [Octet] -> IO [Octet]
getChunk hsh = do
	let chunkKey = hashToHex hsh
	chunk <- getFile (joinPath ["store", (Prelude.take 2 chunkKey), (Prelude.drop 2 chunkKey)])
	return (BS.unpack chunk)

getFileContents :: [Octet] -> IO [Octet]
getFileContents hsh = do
	chunk <- getChunk hsh
	let pieces = splitFor hashSize chunk
	morechunks <- if (Prelude.length pieces) == 1024 then
		getFileContents (Prelude.last pieces)
		else return []
	let morechunks' = splitFor chunkSize morechunks
	conts <- mapM (getChunk) (pieces ++ morechunks')
	return (Prelude.foldl1 (++) conts)

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