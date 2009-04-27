module Hellnet.Files (insertFile) where

import Data.ByteString as BS
import Data.Maybe
import Hellnet
import Hellnet.Storage
import Codec.Utils

insertFile :: FilePath -> IO [Octet]
insertFile fname = do
	conts <- BS.readFile fname
	hsh <- insertFileContents (BS.unpack conts)
	return hsh