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
import Data.ByteString as BS hiding (length, head)
import Data.ByteString.Char8 as BS8 hiding (length, head)
import System.IO.Error
import Data.Either

main = do
	args <- getArgs
	if (or [((length args) < 2), ((length (args !! 1)) /= 128)]) then do
			Prelude.putStrLn "Usage: hell-get {file,chunk} <hash>"
		else do
			let hsh = hexToHash (args !! 1)
			if (head args) == "chunk" then do
				let getConts = findChunk hsh
				conts <- getConts
				maybe (error "Chunk not found in network") (BS.putStr) conts
				else do
				let getFile = findFile hsh
				fil <- getFile
				maybe (error "File couldn't be completely found in network") (BS.putStr) fil