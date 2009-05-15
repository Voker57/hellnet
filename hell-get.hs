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

import Hellnet.Storage
import Hellnet.Utils
import Hellnet.Network
import System.Environment (getArgs)
import Data.Char (chr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.IO.Error
import Data.Either
import Text.Regex.Posix
import Control.Monad
import Data.List

main = do
	args <- getArgs
	if Prelude.null args then
		Prelude.putStrLn "Usage: hell-get <hell:// url>"
		else do
			let arg = head args
			let urlRegex = "^hell://(chunk|file)/([a-f0-9]+)$"
			let encryptedUrlRegex = "^hell://(chunk|file)/([a-f0-9]+)\\.([a-f0-9]+)$"
			when (and [(not (arg =~ urlRegex)), (not (arg =~ encryptedUrlRegex))]) (error "Incorrect hell:// url")
			let parts = head (if arg =~ urlRegex then arg =~ urlRegex else arg =~ encryptedUrlRegex) :: [String]
			let what = parts !! 1
			let hsh = hexToHash (parts !! 2)
			let key = if (length parts) == 4 then Just $ hexToHash $ last parts else Nothing
			if (what == "chunk") then do
				let getConts = findChunk key hsh
				conts <- getConts
				maybe (error "Chunk not found in network") (BS.putStr) conts
				else do
				let getFile = findFile key hsh
				fil <- getFile
				either (\nf -> (error ("File couldn't be completely found in network. Not found chunks: " ++ (intercalate "\n" (map (hashToHex) nf))) )) (BS.putStr) fil