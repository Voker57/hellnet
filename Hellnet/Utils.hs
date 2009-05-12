module Hellnet.Utils (hashToHex, hexToHash, splitFor, stringToOctets, filt, filtM, unjust)  where

import Codec.Text.Raw
import Codec.Utils
import Numeric
import Text.PrettyPrint.HughesPJ (render)
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import qualified Data.ByteString as BS (unpack,pack)
import Data.Foldable (foldlM)

unjust :: (Maybe a) -> a
unjust (Just a) = a

hashToHex :: [Octet] -> String
hashToHex x = render (hexdumpBy "" 666 x)

hexToHash :: String -> [Octet]
hexToHash s = map (fst . head . readHex) (splitFor 2 s)

splitFor :: Int -> [a] -> [[a]]
splitFor _ [] = []
splitFor n xs = (take n xs) : (splitFor n (drop n xs))

stringToOctets :: String -> [Octet]
stringToOctets s = BS.unpack $ BS8.pack s

-- | filters list of values through list of filters
filt fs xs = foldl (flip ($)) xs fs

-- | filt for monads
filtM fs xs = foldlM (flip ($)) xs fs