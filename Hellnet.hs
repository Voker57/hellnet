module Hellnet (insertFile) where

import Data.ByteString as BS
import Data.Digest.SHA512 as SHA512
import Codec.Utils
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import Data.Maybe
import Control.Monad
import System.Directory
import System.FilePath
import Hellnet.Utils
import Data.Foldable

hashSize = 64
chunkSize = 64 * 1024
hashesPerChunk = 1023

insertFile :: FilePath -> IO (Maybe String)
insertFile fname = do
	conts <- catch (fmap Just (BS.readFile fname)) (\e -> return Nothing )
	let result = if isNothing conts then
		Nothing
		else
		Just "wtf"
	return result

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