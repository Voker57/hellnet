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

import Codec.Utils
import Data.Maybe
import Hellnet
import Hellnet.Storage
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

insertFile :: Maybe Key -> FilePath -> IO [Octet]
insertFile encKey fname  = do
	conts <- BSL.readFile fname
	hsh <- insertFileContents encKey conts
	return hsh

getChunkAppendToFile :: Maybe Key -> FilePath -> Hash -> IO ()
getChunkAppendToFile encKey fname hsh = do
	conts <- getChunk encKey hsh
	maybe (error ("chunk not found in storage: " ++ (hashToHex hsh))) (BS.appendFile fname) conts

downloadFile :: Maybe Key -> FilePath -> [Hash] -> IO ()
downloadFile encKey fname hs = do
	mapM (getChunkAppendToFile encKey fname) hs
	return ()