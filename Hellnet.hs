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

module Hellnet where

import Data.Word
import Codec.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

type Hash = [Octet]
type Chunk = BSL.ByteString
type Key = [Octet]
type Node = (String, Int)
type NodeID = Hash
type KeyID = Hash
type Signature = BSL.ByteString

hashSize :: Integer
hashSize = 64

chunkSize :: Integer
chunkSize = 256 * 1024

hashesPerChunk :: Integer
hashesPerChunk = (chunkSize `div` hashSize) - 1

encKeySize :: Integer
encKeySize = 32

numThreads :: Integer
numThreads = 4

xxTeaKey :: (Word32, Word32, Word32, Word32)
xxTeaKey = (104,101,108,108)