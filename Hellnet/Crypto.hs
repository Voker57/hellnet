module Hellnet.Crypto (decryptAES, encryptAES, generateKeyPair) where

import Codec.Encryption.AES
import Codec.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.LargeWord
import Hellnet
import OpenSSL.DSA
import OpenSSL
import System.Posix.Files

decryptAES :: [Octet] -> [Octet] -> [Octet]
decryptAES key os = listToOctets $ map (decrypt ((fromOctets 256 key) :: Word256)) $ listFromOctets os

encryptAES :: [Octet] -> [Octet] -> [Octet]
encryptAES key os = listToOctets $ map (encrypt ((fromOctets 256 key) :: Word256)) $ listFromOctets os

generateKeyPair :: IO DSAKeyPair
generateKeyPair = withOpenSSL $ generateDSAParametersAndKey 2048 Nothing

keyPairFingerprint :: DSAKeyPair -> [Octet]
keyPairFingerprint = undefined