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
import System.Environment (getArgs)
import System.FilePath
import System.IO

insertFilePrintHash :: Maybe [Octet] -> FilePath  -> IO ()
insertFilePrintHash encKey fname = do
	let filename = last (splitPath fname)
	siz <- catch (withFile fname ReadMode (hFileSize)) (const $ return $ (fromIntegral chunkSize) + 1)
	if siz <= (fromIntegral chunkSize) then do
		dat <- BS.readFile fname
		hsh <- insertChunk encKey (BS.unpack dat)
		maybe (putStrLn (fname ++ ": hell://chunk/" ++ (hashToHex hsh) ++ "/" ++ filename )) (\k -> putStrLn (fname ++ ": hell://chunk/" ++ (hashToHex hsh) ++ "." ++ (hashToHex k) ++ "/" ++ filename)) encKey
		else do
		hsh <- insertFile encKey fname
		maybe (putStrLn (fname ++ ": hell://file/" ++ (hashToHex hsh) ++ "/" ++ filename )) (\k -> putStrLn (fname ++ ": hell://file/" ++ (hashToHex hsh) ++ "." ++ (hashToHex k) ++ "/" ++ filename)) encKey

main = do
	argz <- getArgs
	let (opts, args) = simpleOpts argz
	when (length args == 0) (putStrLn ("Usage: hell-insert <file1> [<file2>...]\n" ++
		"Options: -e -- encrypt file\n" ++
			"-k -- use key specified as last parameter for encryption, instead of random one"))
	let encKey = if "-k" `elem` opts then return $ BS.unpack $ BS8.pack $ last args else genKey

	let files = if "-k" `elem` opts then init args else args

	if "-e" `elem` opts then do
		key <- encKey
		mapM (insertFilePrintHash (Just key)) files
		else
		mapM (insertFilePrintHash Nothing) files