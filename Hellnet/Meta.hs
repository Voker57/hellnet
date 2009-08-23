module Hellnet.Meta (Meta(..), MetaMail(..), metaFromTuple, metaFromString, metaMailFromByteString, metaToString, metaMailToByteString) where

import Data.Char
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Hellnet.Utils
import Hellnet

data Meta = Meta String String deriving Show -- key value

data MetaMail = MetaMail Meta [Hash]

metaFromTuple :: (String, String) -> Meta
metaFromTuple (k, v) = Meta k v

metaFromString :: String -> Meta
metaFromString s = let (a, b) = splitInTwo ':' s in Meta a b

metaMailFromByteString :: BS.ByteString -> MetaMail
metaMailFromByteString bs = let
	bsNewline = fromIntegral (ord '\n') :: Word8
	meta = metaFromString $ BS8.unpack $ BS.takeWhile (/= bsNewline) bs;
	hashes = splitFor hashSize $ tail $ BS.unpack $ BS.dropWhile (/= bsNewline) bs
		in MetaMail meta hashes

metaMailToByteString :: MetaMail -> BS.ByteString
metaMailToByteString (MetaMail m hs) = BS.concat (BS8.pack (metaToString m) : BS8.pack "\n" : map (BS.pack) hs)

metaToString :: Meta -> String
metaToString (Meta k v) = concat [k,":",v]