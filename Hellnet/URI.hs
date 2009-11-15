module Hellnet.URI (parseHellnetURI, HellnetURI(..)) where

import Data.List
import Data.Maybe
import Hellnet
import Hellnet.Utils
import Network.URI
import Text.ParserCombinators.Parsec

data HellnetURI =
	ChunkURI Hash -- ^ Chunk hash
		(Maybe Key) -- ^ Encryption key
	| FileURI Hash -- ^ Filelink's start hash
		(Maybe Key) -- ^ Encryption key
	| MetaURI KeyID -- ^ Meta public key ID
		String -- ^ Meta name
		String -- ^ Meta path
			deriving (Show, Eq)


parseHellnetURI :: String -> Maybe HellnetURI
parseHellnetURI s = let
	parsed = parseURI s in
		maybe (Nothing) (parseHellnetURI') parsed

parseHellnetURI' :: URI -> Maybe HellnetURI
parseHellnetURI' u = let
	params = either (const []) (id) $ parse urlEncoded "user URI" (safeTail $ uriQuery u) in
		if uriScheme u == "hell:" && isJust (uriAuthority u) then
			let key = maybe (Nothing) (Just . hexToHash) $ lookup "key" params in
				case uriRegName $ fromJust (uriAuthority u) of
					"chunk" -> let hsh = hexToHash $ safeTail $ uriPath u in
							Just $ ChunkURI hsh key
					"file" -> let hsh = hexToHash $ safeTail $ uriPath u in
							Just $ FileURI hsh key
					"meta" -> let splitPath = explode '/' $ safeTail $ uriPath u in
						if length splitPath < 2 then Nothing
							else
							let keyId = hexToHash $ head splitPath;
								metaName = splitPath !! 1;
								metaPath = intercalate "/" $ drop 2 splitPath in
									Just $ MetaURI keyId metaName metaPath
			else Nothing

-- Really simple query pairs' parser, because urlencoded sucks balls

urlEncoded = encPair `sepBy` char '&'

encPair = do
	first <- manyTill anyChar (char '=')
	second <- many anyChar
	return (first, second)