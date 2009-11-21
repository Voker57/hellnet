module Hellnet.URI (parseHellnetURI, HellnetURI(..)) where

import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Utils
import Network.URI
import Safe
import Text.ParserCombinators.Parsec
import Text.Show

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
			(ChunkURI hsh key fname) -> URI {
				uriScheme = "hell:"
				, uriAuthority = Just URIAuth { uriUserInfo = "", uriPort = "", uriRegName = "//chunk" }
				, uriPath = "/" ++ hashToHex hsh
				, uriQuery = queryFromPairs ([] ++ maybeToPairs key (hashToHex) "key"  ++ maybeToPairs fname (id) "name")
				, uriFragment = ""
				}
			(FileURI hsh key fname) -> URI {
				uriScheme = "hell:"
				, uriAuthority = Just URIAuth { uriUserInfo = "", uriPort = "", uriRegName = "//chunk" }
				, uriPath = "/" ++ hashToHex hsh
				, uriQuery = queryFromPairs $ [] ++ maybeToPairs key (hashToHex) "key" ++ maybeToPairs fname (id) "name"
				, uriFragment = ""
				}
			(MetaURI kid mname mpath key fname) -> URI {
				uriScheme = "hell:"
				, uriAuthority = Just URIAuth { uriUserInfo = "", uriPort = "", uriRegName = "//meta" }
				, uriPath = "/" ++ intercalate "/" [hashToHex kid, mname, mpath]
				, uriQuery = queryFromPairs $ [] ++ maybeToPairs key (hashToHex) "key" ++ maybeToPairs fname (id) "name"
				, uriFragment = ""
				}
	showList = showListWith (const show)

parseHellnetURI :: String -> Maybe HellnetURI
parseHellnetURI s = let
	parsed = parseURI s in
		maybe (Nothing) (parseHellnetURI') parsed

parseHellnetURI' :: URI -> Maybe HellnetURI
parseHellnetURI' u = let
	params = either (const []) (id) $ parse urlEncoded "user URI" (tailSafe $ uriQuery u) in
		if uriScheme u == "hell:" && isJust (uriAuthority u) then
			let key = maybe (Nothing) (Just . hexToHash) $ lookup "key" params;
				fname = lookup "name" params in
					case uriRegName $ fromJust (uriAuthority u) of
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
			else Nothing

-- Really simple query pairs' parser, because urlencoded sucks balls

urlEncoded = encPair `sepBy` char '&'

encPair = do
	first <- manyTill anyChar (char '=')
	second <- many anyChar
	return (first, second)

-- constructing queries

queryFromPairs :: [(String, String)] -> String
queryFromPairs xs = "?" ++ initSafe (foldl (\s p -> s ++ fst p ++ "=" ++ snd p ++ "&") "" xs)

-- this is for constructing pairlists from maybes
maybeToPairs :: Maybe a -> (a -> String) -> String -> [(String, String)]
maybeToPairs a f name = maybe ([]) (\x -> [(uriescape name, uriescape (f x))]) a

uriescape = escapeURIString (isUnescapedInURI)