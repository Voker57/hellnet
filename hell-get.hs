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
import Data.Char (chr)
import Data.Either
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
import Text.Regex.Posix

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
		Prelude.putStrLn "Usage: hell-get <hell:// url>"
		else do
			let arg = head argz
			let urlRegex = "^hell://(chunk|file)/([a-f0-9]+)(:?/([^/]+))?$"
			let encryptedUrlRegex = "^hell://(chunk|file)/([a-f0-9]+)\\.([a-f0-9]+)(:?/([^/]+))?$"
			when (and [(not (arg =~ urlRegex)), (not (arg =~ encryptedUrlRegex))]) (error "Incorrect hell:// url")
			let uri = parseHellnetURI $ head argz
			let parts = head (if arg =~ urlRegex then arg =~ urlRegex else arg =~ encryptedUrlRegex) :: [String]
			let what = parts !! 1
			let hsh = hexToHash (parts !! 2)
			let key = if arg =~ encryptedUrlRegex then Just $ hexToHash $ parts !! 3 else Nothing
			let fname = if (length argz) == 2 then last argz
				else if and [(arg =~ encryptedUrlRegex), (not (null (last parts)))] then
					last parts
					else if and [(arg =~ urlRegex), (not (null (last parts)))] then
						last parts
						else "/dev/stdout"
			if (what == "chunk") then do
				let getConts = findChunk key hsh
				conts <- getConts
				maybe (error "Chunk not found in network") (BS.writeFile fname) conts
				else do
				fil <- fetchFile key hsh
				either (\nf -> (error ("File couldn't be completely found in network. Not found chunks: " ++ (intercalate "\n" (map (hashToHex) nf))) )) (\hs -> do
					downloadFile key fname hs
					when (deintegrateFile opts) (do
						toDelete <- filterM (const $ do
							rnd <- randomIO :: IO Float
							return (rnd > 0.8)
							) hs
						mapM_ (purgeChunk) toDelete
						)
					return ()
					) fil