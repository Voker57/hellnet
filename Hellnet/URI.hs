module Hellnet.URI (parseHellnetURI, HellnetURI(..)) where

import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Utils
import Safe
import Text.ParserCombinators.Parsec
import Text.Show
import Text.URI

data HellnetURI =
	ChunkURI Hash -- ^ Chunk hash
		(Maybe Key) -- ^ Encryption key
		(Maybe String) -- ^ File name
	| FileURI Hash -- ^ Filelink's start hash
		(Maybe Key) -- ^ Encryption key
		(Maybe String) -- ^ File name
	| MetaURI KeyID -- ^ Meta public key ID
		String -- ^ Meta name
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
				, uriPath = "/" ++ hashToHex hsh
				, uriQuery = let ps = ([] ++ maybeToPairs key (hashToHex) "key"  ++ maybeToPairs fname (id) "name") in if null ps then Nothing else Just (pairsToQuery ps)
				}
			(FileURI hsh key fname) -> nullURI {
				uriScheme = Just "hell"
				, uriRegName = Just "file"
				, uriPath = "/" ++ hashToHex hsh
				, uriQuery = let ps = ([] ++ maybeToPairs key (hashToHex) "key"  ++ maybeToPairs fname (id) "name") in if null ps then Nothing else Just (pairsToQuery ps)
				}
			(MetaURI kid mname mpath key fname) -> nullURI {
				uriScheme = Just "hell"
				, uriRegName = Just "meta"
				, uriPath = "/" ++ intercalate "/" [hashToHex kid, mname, mpath]
				, uriQuery = let ps = ([] ++ maybeToPairs key (hashToHex) "key"  ++ maybeToPairs fname (id) "name") in if null ps then Nothing else Just (pairsToQuery ps)
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
			let key = maybe (Nothing) (Just . hexToHash) $ lookup "key" params;
				fname = lookup "name" params in
					case fromMaybe "" (uriRegName u) of
						"chunk" -> let hsh = hexToHash $ tailSafe $ uriPath u in
								Just $ ChunkURI hsh key fname
						"file" -> let hsh = hexToHash $ tailSafe $ uriPath u in
								Just $ FileURI hsh key fname
						"meta" -> let splitPath = explode '/' $ tailSafe $ uriPath u in
							if length splitPath < 2 then Nothing
								else
								let keyId = hexToHash $ head splitPath;
									metaName = splitPath !! 1;
									metaPath = intercalate "/" $ drop 2 splitPath in
										Just $ MetaURI keyId metaName metaPath key fname
						otherwise -> Nothing
			else Nothing

-- this is for constructing pairlists from maybes
maybeToPairs :: Maybe a -> (a -> String) -> String -> [(String, String)]
maybeToPairs a f name = maybe ([]) (\x -> [(uriescape name, uriescape (f x))]) a

uriescape = escapeString (okInQuery)