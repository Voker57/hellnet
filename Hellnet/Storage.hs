module Hellnet.Storage (insertFileContents, insertChunk, storeFile) where

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
		let fullpath = joinPath ["store", (Prelude.take 2 chunkDigest), (Prelude.drop 2 chunkDigest)]
		storeFile fullpath (BS.pack chunk)
		return chunkDigestRaw

storeFile :: FilePath -> ByteString -> IO ()
storeFile fpath dat = do
	dir <- getAppUserDataDirectory "hellnet"
	let fullPath = joinPath [dir,fpath]
	createDirectoryIfMissing True (dropFileName fullPath)
	BS.writeFile fullPath dat