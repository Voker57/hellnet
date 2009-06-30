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
import Hellnet.Files
import Hellnet.Meta
import Hellnet.Storage
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath
import System.IO

data Opts = Opts {encKey :: (Maybe String), encrypt :: Bool, meta :: [Meta], chunk :: Bool}

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['k'] ["key"]
		(ReqArg (\s o -> o {encKey = Just s}) "key") "Encrypt with specified key",
	Option ['e'] ["encrypt"]
		(NoArg (\o -> o {encrypt = True})) "Encrypt file",
	Option ['m'] ["meta"]
		(ReqArg (\s o -> o {meta = let sp = splitInTwo ':' s in (Meta (fst sp) (snd sp)) : (meta o)}) "key:value") "Add file with specified meta",
	Option ['c'] ["chunk"]
		(NoArg (\o -> o {chunk = True})) "Add file as single chunk (Only for files < 256 kB)"
	]

defaultOptions = Opts {encKey = Nothing, encrypt = False, meta = [], chunk = False}


insertFilePrintHash :: Maybe [Octet] -> [Meta] -> FilePath -> IO ()
insertFilePrintHash encKey metas fname = do
	let filename = last (splitPath fname)
	hsh <- insertFile encKey fname
	maybe (putStrLn (fname ++ ": hell://file/" ++ (hashToHex hsh) ++ "/" ++ filename )) (\k -> putStrLn (fname ++ ": hell://file/" ++ (hashToHex hsh) ++ "." ++ (hashToHex k) ++ "/" ++ filename)) encKey
	addHashToMetas hsh metas

insertChunkPrintHash :: Maybe [Octet] -> FilePath -> IO ()
insertChunkPrintHash encKey fname = do
	let filename = last (splitPath fname)
	dat <- BS.readFile fname
	when (BS.length dat > chunkSize) (fail $ "Too large to insert as chunk: " ++ fname)
	hsh <- insertChunk encKey (BS.unpack dat)
	maybe (putStrLn (fname ++ ": hell://chunk/" ++ (hashToHex hsh) ++ "/" ++ filename )) (\k -> putStrLn (fname ++ ": hell://chunk/" ++ (hashToHex hsh) ++ "." ++ (hashToHex k) ++ "/" ++ filename)) encKey

main = do
	args <- getArgs
 	let (opts, argz, errs) = getOpt Permute options args
	let optz = processOptions defaultOptions opts
	when (or [(not . null $ errs), (null argz)]) (fail $ (usageInfo "Usage: hell-insert [file] file1 [file2...]" options) ++ concat errs)
	theKey <- maybe (genKey) (return . BS.unpack . BS8.pack) (encKey optz)
	if chunk optz then
		mapM (insertChunkPrintHash (if encrypt optz then (Just $ theKey) else Nothing)) argz
		else
		mapM (insertFilePrintHash (if encrypt optz then (Just $ theKey) else Nothing) (meta optz)) argz