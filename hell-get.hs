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

import Control.Monad
import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Files
import Hellnet.Network
import Hellnet.Storage
import Hellnet.URI
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Safe
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.IO.Error
import System.Random
import Text.Printf

data Opts = Opts { deintegrateFile :: Bool, decryptUri :: Bool }

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['d'] ["deintegrate"]
		(NoArg (\o -> o {deintegrateFile = True}) ) "Remove random chunks of file after fetching (increases deniability, decreases seedability)",
	Option ['p'] ["decrypt-uri", "decrypt-url"]
		(NoArg (\o -> o {decryptUri = True})) "Decrypt given crypted URIs"
	]

defaultOptions = Opts { deintegrateFile = False, decryptUri = False }

getURI opts uri = do
	let getOutputHandle fnameUnsafe = do
		let fname = fmap (lastDef "file.dat" . explode '/') fnameUnsafe
		case fname of
			Just filename -> do
				hPutStrLn stderr $ printf "Saving to file: %s" filename
				openFile filename WriteMode
			Nothing -> do
				hPutStrLn stderr "Saving to stdin"
				return stdin
	case uri of
		Nothing -> fail "Incorrect URI"
		Just (ChunkURI hsh key fnameUnsafe) -> do
			fileHandle <- getOutputHandle fnameUnsafe
			conts <- findChunk key hsh
			maybe (fail "Chunk not found in network") (\bs -> BSL.hPutStr fileHandle bs >> hClose fileHandle) conts
		Just (FileURI hsh key fnameUnsafe) -> do
			fileHandle <- getOutputHandle fnameUnsafe
			fil <- fetchFile key hsh
			either (\nf -> (error ("File couldn't be completely found in network. Not found chunks: " ++ (intercalate "\n" (map (crockford) nf))) )) (\hs -> do
				downloadFileChunksToHandle key fileHandle hs
				hClose fileHandle
				when (deintegrateFile opts) (do
					toDelete <- filterM (const $ do
						rnd <- randomIO :: IO Float
						return (rnd > 0.8)
						) hs
					mapM_ (purgeChunk) toDelete
					)
				return ()
				) fil
		Just u@(MetaURI _ _ _ _ fnameUnsafe) -> do
			fileHandle <- getOutputHandle fnameUnsafe
			resultM <- findURI u
			maybe (fail "Not found") (\bs -> BSL.hPutStr fileHandle bs >> hClose fileHandle) resultM
		Just u@(CryptURI dt) -> do
			let decryptedUri = Hellnet.URI.decryptURI u
			when (isJust decryptedUri) $ hPutStr stderr $ printf "Actual URI is %s\n" (show $ fromJust decryptedUri)
			getURI opts decryptedUri
		otherwise -> fail "URI type not implemented"

main = do
	args <- getArgs
	let (optz, argz, errs) = getOpt Permute options args
	let opts = processOptions defaultOptions optz
	if Prelude.null argz then
		Prelude.putStrLn "Usage: hell-get <hell:// uri>"
		else if decryptUri opts then
			mapM_ (\u -> case u of Nothing -> putStrLn ""; Just Nothing -> putStrLn ""; Just (Just uV) -> print uV) $ map (fmap (decryptURI) . parseHellnetURI) argz
			else do
			let uri = parseHellnetURI $ head argz
			getURI opts uri
				