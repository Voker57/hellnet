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

import Codec.Utils

type Hash = [Octet]
type Chunk = [Octet]
type Key = [Octet]

data Meta = Meta String String deriving Show -- key value

hashSize :: Int
hashSize = 64

chunkSize :: Int
chunkSize = 256 * 1024

hashesPerChunk :: Int
hashesPerChunk = (chunkSize `div` hashSize) - 1

encKeySize :: Int
encKeySize = 32

numThreads :: Int
numThreads = 4