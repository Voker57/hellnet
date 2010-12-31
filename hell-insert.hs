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

import Codec.Utils
import Control.Monad
import Hellnet
import Hellnet.ExternalChunks
import Hellnet.Files
import Hellnet.Storage
import Hellnet.URI
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO

data Opts = Opts {encKey :: Maybe Key, encrypt :: Bool,  chunk :: Bool, indexOnly :: Bool, crypt :: Bool, cryptOnly :: Bool}

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['e'] ["encrypt"]
		(OptArg (\s o -> o {encKey = maybe (Nothing) (Just . decrockford) s, encrypt = True}) "key") "Encrypt (optionally with specified key)",
	Option ['c'] ["chunk"]
		(NoArg (\o -> o {chunk = True})) "Add file as single chunk (Only for files < 256 kB)",
	Option ['i'] ["index-only"]
		(NoArg (\o -> o {indexOnly = True})) "Do not insert chunks into store, but add them into chunks' map instead, so hell-serve would get them out of file directly. Much slower. Probably.",
	Option ['p'] ["crypt"]
		(NoArg (\o -> o {crypt = True})) "Encrypt URI with XXTEA, which will help to notice if URI was tampered with and hide its contents from first sight",
	Option [] ["crypt-only"]
		(NoArg (\o -> o {cryptOnly = True})) "Encrypt given URIs with XXTEA (see above)"
	]

defaultOptions = Opts {encKey = Nothing, encrypt = False, chunk = False, indexOnly = False, crypt = False, cryptOnly = False}


insertFilePrintHash :: Maybe [Octet] ->  FilePath -> IO HellnetURI
insertFilePrintHash encKey fname = do
	let filename = last (splitPath fname)
	hsh <- insertFile encKey fname
	let url = FileURI hsh encKey (Just filename)
	return url

insertChunkPrintHash :: Maybe [Octet] -> FilePath -> IO HellnetURI
insertChunkPrintHash encKey fname = do
	let filename = last (splitPath fname)
	dat <- BSL.readFile fname
	when (not $ BSL.null $ BSL.drop (fromIntegral chunkSize) dat) (fail $ "Too large to insert as chunk: " ++ fname)
	hsh <- insertChunk encKey dat
	let
	let url = ChunkURI hsh encKey (Just filename)
	return url

indexFilePrintHash :: Maybe [Octet] ->  FilePath -> IO HellnetURI
indexFilePrintHash encKey fname = do
	let filename = last (splitPath fname)
	hsh <- indexFile encKey fname
	let url = FileURI hsh encKey (Just filename)
	return url

indexChunkPrintHash :: Maybe [Octet] -> FilePath -> IO HellnetURI
indexChunkPrintHash encKey fname = do
	let filename = last (splitPath fname)
	fullPath <- canonicalizePath fname
	dat <- BSL.readFile fname
	when (not $ BSL.null $ BSL.drop (fromIntegral chunkSize) dat) (fail $ "Too large to insert as chunk: " ++ fname)
	hsh <- indexChunk encKey dat (FileLocation fullPath 0 encKey)
	let url = ChunkURI hsh encKey (Just filename)
	return url

main = do
	args <- getArgs
 	let (opts, argz, errs) = getOpt Permute options args
	let optz = processOptions defaultOptions opts
	when (or [(not . null $ errs), (null argz)]) (fail $ (usageInfo "Usage: hell-insert [file] file1 [file2...]" options) ++ concat errs)
	if cryptOnly optz then do
		mapM (\s -> do
			case parseHellnetURI s of
				Nothing -> putStrLn ""
				Just (CryptURI _) -> putStrLn ""
				Just u -> print $ encryptURI u
			) argz
		else do
		theKey <- if encrypt optz then
			maybe (genKey >>= return . Just) (return . Just) (encKey optz)
			else
			return Nothing
		urls <- if chunk optz then
			if indexOnly optz then
				mapM (indexChunkPrintHash theKey) argz
				else
				mapM (insertChunkPrintHash theKey) argz
			else
			if indexOnly optz then
				mapM (indexFilePrintHash theKey) argz
				else do
				mapM (insertFilePrintHash theKey) argz
		mapM (\u -> if crypt optz then print $ encryptURI u else print u) urls