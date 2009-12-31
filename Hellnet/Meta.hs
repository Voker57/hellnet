module Hellnet.Meta (Meta(..), byteStringToMeta) where

import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BU
import Data.Maybe
import Debug.Trace
import Hellnet
import Hellnet.Utils
import Hellnet.URI
import Text.HJson as Json
import Text.JSON.JPath

data Meta = Meta {
	keyID :: KeyID -- ^ Public key ID
	, metaName :: String -- ^ Meta name
	, timestamp :: Integer -- ^ Updated timestamp
	, contentURI :: HellnetURI -- ^ Meta content location
	, signature :: Signature -- ^ Digital signature
	} deriving (Eq, Show)

byteStringToMeta :: BS.ByteString -> Maybe Meta
byteStringToMeta bs = let
	(s, sig) = BS.breakSubstring (BS8.pack "\n\n") bs;
	js = BU.toString s in
		maybe (Nothing) (\v -> let
			keyIdV = jPath' "key" v;
			timestampV = jPath' "updated" v;
			nameV = jPath' "name" v;
			contentV = jPath' "content" v in
				case [keyIDV, timestampV, nameV, contentV] of
					[	[JString keyHash]
						, [JNumber timestampF]
						, [JString nameS]
						, [JString contentURI]
						] -> maybe (Nothing) (\u -> Just $ Meta {
							keyID = hexToHash keyHash
							, metaName = nameS
							, timestamp = (round timestampF)
							, contentURI = u
							, signature = sig) (traceShow (parseHellnetURI contentURI) (parseHellnetURI contentURI))
					otherwise -> Nothing) $ Json.fromString js

verifyMeta :: BS.ByteString -> IO Bool
verifyMeta bs = undefined
 	let (dat, sig) = BS.breakSubstring (BS8.pack "\n\n") bs
 	sigKey <- findChunk ()