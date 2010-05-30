module Hellnet.Meta (Meta(..), fromByteString, toByteString, emptyMeta) where

import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.Map as Map
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
	, message :: Maybe BSL.ByteString -- ^ JSON as ByteString
	, signature :: Maybe Signature -- ^ Digital signature
	} deriving (Eq, Show)

emptyMeta = Meta {
	keyID = [] -- ^ Public key ID
	, metaName = "" -- ^ Meta name
	, timestamp = 0 -- ^ Updated timestamp
	, contentURI = ChunkURI [] Nothing Nothing -- ^ Meta content location
	, message = Nothing -- ^ JSON as ByteString
	, signature = Nothing -- ^ Digital signature
}

instance Jsonable Meta where
	fromJson v = let
		keyIDV = jPath "key" v;
		timestampV = jPath "updated" v;
		nameV = jPath "name" v;
		contentV = jPath "content" v in
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
						, signature = Nothing
						, message = Nothing}) (parseHellnetURI contentURI)
				otherwise -> Nothing
	toJson m = toJson $ Map.fromList [
		("key", toJson $ hashToHex $ keyID m)
		, ("updated", toJson $ timestamp m)
		, ("name", toJson $ metaName m)
		, ("content", toJson $ show $ contentURI m)
		]

fromByteString :: BSL.ByteString -> Maybe Meta
fromByteString bs = let
	(s, sigTail) = breakLazySubstring (BSL8.pack "\n\n") bs;
	sig = BSL.drop 2 sigTail
	js = BUL.toString s
	pM = either (const Nothing) (fromJson) $ Json.fromString js in
		case pM of
			Nothing -> Nothing
			Just preMeta -> Just $ preMeta { message = Just s, signature = Just sig}

toByteString :: Meta -> BSL.ByteString
toByteString m = case (message m, signature m) of
	(Just msg, Just sig) -> BSL.concat [msg, BSL8.pack "\n\n", sig]
	otherwise -> BSL.empty