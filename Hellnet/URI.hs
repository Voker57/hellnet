module Hellnet.URI (parseHellnetURI, HellnetURI(..), decryptURI, encryptURI) where

import Codec.Encryption.XXTEA as XXTEA
import Codec.Utils
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Utils
import Safe
import Text.ParserCombinators.Parsec
import Text.Show
import Text.URI

data HellnetURI =
	CryptURI [Octet] -- ^ Encrypted content URI
	| ChunkURI Hash -- ^ Chunk hash
		(Maybe Key) -- ^ Encryption key
		(Maybe String) -- ^ File name
	| FileURI Hash -- ^ Filelink's start hash
		(Maybe Key) -- ^ Encryption key
		(Maybe String) -- ^ File name
	| MetaURI KeyID -- ^ Meta public key ID
		(Maybe String) -- ^ Meta name
		String -- ^ Meta path
		(Maybe Key) -- ^ Encryption key
		(Maybe String) -- ^ File name
			deriving (Eq)

instance Show HellnetURI where
	show u = show uri where
		uri = case u of
			(ChunkURI hsh key fname) -> nullURI {
				uriScheme = Just "hell"
				, uriRegName = Just "chunk"
				, uriPath = "/" ++ crockford hsh
				, uriQuery = let ps = ([] ++ maybeToPairs key (crockford) "key"  ++ maybeToPairs fname (id) "name") in if null ps then Nothing else Just (pairsToQuery ps)
				}
			(FileURI hsh key fname) -> nullURI {
				uriScheme = Just "hell"
				, uriRegName = Just "file"
				, uriPath = "/" ++ crockford hsh
				, uriQuery = let ps = ([] ++ maybeToPairs key (crockford) "key"  ++ maybeToPairs fname (id) "name") in if null ps then Nothing else Just (pairsToQuery ps)
				}
			(MetaURI kid mname mpath key fname) -> nullURI {
				uriScheme = Just "hell"
				, uriRegName = Just "meta"
				, uriPath = "/" ++ crockford kid ++ "/" ++ case mname of
					Just mn -> intercalate "/" [mn, mpath]
					otherwise -> if null mpath then [] else mpath
				, uriQuery = let ps = ([] ++ maybeToPairs key (crockford) "key"  ++ maybeToPairs fname (id) "name") in if null ps then Nothing else Just (pairsToQuery ps)
				}
			(CryptURI dat) -> nullURI {
				uriScheme = Just "hell"
				, uriRegName = Just "crypt"
				, uriPath = "/" ++ crockford dat
				}
	showList = showListWith (const show)

parseHellnetURI :: String -> Maybe HellnetURI
parseHellnetURI s = let
	parsed = parseURI s in
		maybe (Nothing) (parseHellnetURI') parsed

parseHellnetURI' :: URI -> Maybe HellnetURI
parseHellnetURI' u = let
	params = uriQueryItems u in
		if uriScheme u == Just "hell" then
			let key = maybe (Nothing) (Just . decrockford) $ lookup "key" params;
				fname = lookup "name" params in
					case fromMaybe "" (uriRegName u) of
						"chunk" -> let hsh = decrockford $ tailSafe $ uriPath u in
								Just $ ChunkURI hsh key fname
						"file" -> let hsh = decrockford $ tailSafe $ uriPath u in
								Just $ FileURI hsh key fname
						"meta" -> let splitPath = explode '/' $ tailSafe $ uriPath u in
							case splitPath of
								(keyid : mname : mpath) -> Just $ MetaURI (decrockford keyid) (Just mname) (intercalate "/" mpath) key fname
								[keyid] -> Just $ MetaURI (decrockford keyid) Nothing "" key fname
								otherwise -> Nothing
						"crypt" -> Just $ CryptURI $ decrockford $ tailSafe (uriPath u)
						otherwise ->  Nothing
			else Nothing

-- | Decrypt crypted URI, or just return it if it's not crypted
decryptURI :: HellnetURI -> Maybe HellnetURI
decryptURI (CryptURI dt) =
	let decryptedDt = XXTEA.decrypt xxTeaKey dt in
		case parseHellnetURI (BS8.toString $ BS.pack decryptedDt)	of
			Just (CryptURI _) -> Nothing -- Really, no need to be so paranoid
			u -> u
decryptURI u = Just u

-- | Encrypts URI
encryptURI :: HellnetURI -> HellnetURI
encryptURI u@(CryptURI _) = u
encryptURI u = CryptURI $ XXTEA.encrypt xxTeaKey $ BS.unpack $ BS8.fromString $ show u

-- this is for constructing pairlists from maybes
maybeToPairs :: Maybe a -> (a -> String) -> String -> [(String, String)]
maybeToPairs a f name = maybe ([]) (\x -> [(uriescape name, uriescape (f x))]) a

uriescape = escapeString (okInQuery)