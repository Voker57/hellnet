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
import Hellnet.Storage
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath
import System.IO

data Opts = Opts {encKey :: (Maybe String), encrypt :: Bool, meta :: [Meta]}

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['k'] ["key"]
		(ReqArg (\s o -> o {encKey = Just s}) "key") "Encrypt with specified key",
	Option ['e'] ["encrypt"]
		(NoArg (\o -> o {encrypt = True})) "Encrypt file",
	Option ['m'] ["meta"]
		(ReqArg (\s o -> o {meta = let sp = splitInTwo ':' s in (Meta (fst sp) (snd sp)) : (meta o)}) "key:value") "Add file with specified meta"
	]

defaultOptions = Opts {encKey = Nothing, encrypt = False, meta = []}


insertFilePrintHash :: Maybe [Octet] -> [Meta] -> FilePath -> IO ()
insertFilePrintHash encKey metas fname = do
	let filename = last (splitPath fname)
	dat <- BS.readFile fname
	hsh <- insertFile encKey fname
	maybe (putStrLn (fname ++ ": hell://file/" ++ (hashToHex hsh) ++ "/" ++ filename )) (\k -> putStrLn (fname ++ ": hell://file/" ++ (hashToHex hsh) ++ "." ++ (hashToHex k) ++ "/" ++ filename)) encKey
	addHashToMetas hsh metas

main = do
	args <- getArgs
 	let (opts, argz, errs) = getOpt Permute options args
	let optz = processOptions defaultOptions opts
	when (or [(not . null $ errs), (null argz)]) (fail $ (usageInfo "Usage: hell-insert [file] file1 [file2...]" options) ++ concat errs)
	theKey <- maybe (genKey) (return . BS.unpack . BS8.pack) (encKey optz)

	mapM (insertFilePrintHash (if encrypt optz then (Just $ theKey) else Nothing) (meta optz)) argz