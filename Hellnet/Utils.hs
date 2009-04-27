module Hellnet.Utils (hashToHex, splitFor)  where

import Codec.Text.Raw
import Codec.Utils
import Text.PrettyPrint.HughesPJ (render)
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)

hashToHex :: [Octet] -> String
hashToHex x = render (hexdumpBy "" 666 x)

splitFor :: Int -> [a] -> [[a]]
splitFor _ [] = []
splitFor n xs = (take n xs) : (splitFor n (drop n xs))