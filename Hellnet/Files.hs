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

module Hellnet.Files (insertFile, downloadFile) where

import qualified Data.ByteString as BS
import Data.Maybe
import Hellnet
import Hellnet.Storage
import Codec.Utils

insertFile :: FilePath -> IO [Octet]
insertFile fname = do
	conts <- BS.readFile fname
	hsh <- insertFileContents conts
	return hsh

getChunkAppendToFile :: FilePath -> [Octet] -> IO ()
getChunkAppendToFile fname hsh = do
	conts <- getChunk hsh
	maybe (return ()) (BS.appendFile fname) conts

downloadFile :: FilePath -> [Octet] -> [[Octet]] -> IO ()
downloadFile fname encKey hs = do
	mapM (getChunkAppendToFile fname) hs
	return ()