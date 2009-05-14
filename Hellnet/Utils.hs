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

module Hellnet.Utils (hashToHex, hexToHash, splitFor, stringToOctets, filt, filtM, unjust, splitBsFor, shuffle, genhash)  where

import Codec.Text.Raw
import Codec.Utils
import Numeric
import Text.PrettyPrint.HughesPJ (render)
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import qualified Data.ByteString as BS
import Data.Foldable (foldlM)
import Random
import Data.List
import Hellnet
import Data.Word

unjust :: (Maybe a) -> a
unjust (Just a) = a

hashToHex :: [Octet] -> String
hashToHex x = render (hexdumpBy "" 666 x)

hexToHash :: String -> [Octet]
hexToHash s = map (fst . head . readHex) (splitFor 2 s)

splitFor :: Int -> [a] -> [[a]]
splitFor _ [] = []
splitFor n xs = (take n xs) : (splitFor n (drop n xs))

-- splitBsFor :: Int -> BS -> [BS]
splitBsFor n xs = if BS.null xs then
	[]
	else
	(BS.take n xs) : (splitBsFor n (BS.drop n xs))

stringToOctets :: String -> [Octet]
stringToOctets s = BS.unpack $ BS8.pack s

-- | filters list of values through list of filters
filt fs xs = foldl (flip ($)) xs fs

-- | filt for monads
filtM fs xs = foldlM (flip ($)) xs fs

-- | shuffles list
shuffle :: [a] -> IO [a]
shuffle xs = do
	gen <- newStdGen
	let zipd = zip xs (take (length xs) ((randoms gen) :: [Float]))
	return (map (fst) (sortBy (\a b -> (snd a) `compare` (snd b)) zipd))

genhash :: IO [Octet]
genhash = do
	gen <- newStdGen
	return (map (fromIntegral) (take hashSize (randomRs (0,255) gen) :: [Int]))