module Hellnet.Crypto (decryptAES, encryptAES, encryptAsym, encryptAsym', decryptAsym, signAsym, verifyAsym) where

import Codec.Crypto.RSA as RSA
import Codec.Encryption.AES as AES
import Codec.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import Data.LargeWord
import System.Random
import qualified Data.Map as Map
import Hellnet
import Text.HJson
import Safe
import System.Posix.Files

decryptAES :: [Octet] -> [Octet] -> [Octet]
decryptAES key os = listToOctets $ map (AES.decrypt ((fromOctets 256 key) :: Word256)) $ listFromOctets os

encryptAES :: [Octet] -> [Octet] -> [Octet]
encryptAES key os = listToOctets $ map (AES.encrypt ((fromOctets 256 key) :: Word256)) $ listFromOctets os

generateKeyPair :: IO (PublicKey, PrivateKey)
generateKeyPair = do
	g <- newStdGen
	let (pub, priv, _) = RSA.generateKeyPair g 1024
	return (pub, priv)

instance Jsonable PublicKey where
	toJson pk = toJson $ Map.fromList [
		("public_size", fromIntegral $ public_size pk)
		, ("public_n", public_n pk)
		, ("public_e", public_e pk)
		]
	fromJson (JObject objMap) =
		let lookups = (Map.lookup "public_size" objMap
			, Map.lookup "public_n" objMap
			, Map.lookup "public_e" objMap) in
				case lookups of
					(Just (JNumber public_sizeR)
						, Just (JNumber public_nR)
						, Just (JNumber public_eR)
						) -> Just PublicKey { public_size = round public_sizeR, public_n = round public_nR, public_e = round public_eR }
					otherwise -> Nothing
	fromJson _ = Nothing

instance Jsonable PrivateKey where
	toJson pk = toJson $ Map.fromList [
		("private_size", fromIntegral $ private_size pk)
		, ("private_n", private_n pk)
		, ("private_d", private_d pk)
		]
	fromJson (JObject objMap) =
		let lookups = (Map.lookup "private_size" objMap
			, Map.lookup "private_n" objMap
			, Map.lookup "private_d" objMap) in
				case lookups of
					(Just (JNumber private_sizeR)
						, Just (JNumber private_nR)
						, Just (JNumber private_dR)
						) -> Just PrivateKey { private_size = round private_sizeR, private_n = round private_nR, private_d = round private_dR }
					otherwise -> Nothing
	fromJson _ = Nothing

encryptAsym :: PublicKey -> BSL.ByteString -> IO BSL.ByteString
encryptAsym pk msg = do
	gen <- newStdGen
	let (res, _) = RSA.encrypt gen pk msg
	return res
encryptAsym' :: RandomGen g => g -> PublicKey -> BSL.ByteString -> (BSL.ByteString, g)
encryptAsym' = RSA.encrypt
decryptAsym = RSA.decrypt
signAsym = RSA.sign
verifyAsym = RSA.verify