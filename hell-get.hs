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
import Hellnet
import Hellnet.Files
import Hellnet.Network
import Hellnet.Storage
import Hellnet.URI
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.IO.Error
import System.Random

data Opts = Opts { deintegrateFile :: Bool }

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['d'] ["deintegrate"]
		(NoArg (\o -> o {deintegrateFile = True}) ) "Remove random chunks of file after fetching (increases deniability, decreases seedability)"
	]

defaultOptions = Opts { deintegrateFile = False }

main = do
	args <- getArgs
	let (optz, argz, errs) = getOpt Permute options args
	let opts = processOptions defaultOptions optz
	if Prelude.null argz then
		Prelude.putStrLn "Usage: hell-get <hell:// uri>"
		else do
			let uri = parseHellnetURI $ head argz
			case uri of
				Nothing -> fail "Incorrect URI"
				Just (ChunkURI hsh key fname) -> do
					let filename = maybe "/dev/stdout" (id) fname
					conts <- findChunk key hsh
					maybe (fail "Chunk not found in network") (BSL.writeFile filename) conts
				Just (FileURI hsh key fname) -> do
					let filename = maybe "/dev/stdout" (id) fname
					fil <- fetchFile key hsh
					either (\nf -> (error ("File couldn't be completely found in network. Not found chunks: " ++ (intercalate "\n" (map (hashToHex) nf))) )) (\hs -> do
					downloadFile key filename hs
					when (deintegrateFile opts) (do
						toDelete <- filterM (const $ do
							rnd <- randomIO :: IO Float
							return (rnd > 0.8)
							) hs
						mapM_ (purgeChunk) toDelete
						)
					return ()
					) fil
				otherwise -> fail "URI type not implemented yet"