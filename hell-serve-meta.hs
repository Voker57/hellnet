import Hellnet.FileTree
import Network.HTTP.Lucu as Lucu
import Hellnet.Network
import Hellnet.URI
import Control.Monad.Trans
import Text.JSON.JPath
import Text.HJson
import System.Environment
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Text.Printf
import Text.URI

pathResource metaJson = do
	path <- getPathInfo

	let currentJPath = map (ObjectLookup) $ if null path then [] else ("contents" :) $ intersperse "contents" path
	let nodeType (JObject m) = (\ (JString a) -> a ) $ fromJust $ Map.lookup "type" m
	case jPath currentJPath metaJson of
		[] -> setStatus NotFound
		[o] -> case fromJson o of
			Just (Dir _ mp) -> do
				let cpath = intercalate "/" path
				let prelude = printf "<html><head><title>%s</title></head><body><h1>Index of /%s</h1>" cpath cpath
				let postlude = "</body></html>"
				let contents = sortBy (\ a b -> case (a, b) of
					((_, Dir _ _), (_, File _ _)) -> LT
					((_, File _ _), (_, Dir _ _)) -> GT
					((name1, _), (name2, _)) -> compare name1 name2
					) $ Map.toList mp
				let content = concat $ map (\ (k, v) ->
					case v of
						Dir _ _ -> printf "<a href='%s/'>%s/</a> <br />" (intercalate "/" ([] : path ++ [k])) k
						File _ _ -> printf "<a href='%s'>%s</a> <br />" (intercalate "/" ([] : path ++ [k])) k ) $ contents
				output $ concat [prelude, content, postlude]
			Just (File _ link) -> do
				result <- liftIO $ findURI (fromMaybe (error "Failed to parse URI") $ parseHellnetURI $ link)
				case result of
					Nothing -> do
						setStatus NotFound
						output "Failed to fetch file from network"
					Just bs ->
						outputLBS bs
			otherwise -> setStatus InternalServerError
		otherwise -> setStatus InternalServerError

main = do
	args <- getArgs
	let port = head args
	let config = defaultConfig { cnfServerPort = port }
	let Just (MetaURI hsh (Just mName) mPath mKey _) = parseHellnetURI $ args !! 1
	[metaJson] <- findMetaContentByName mKey hsh mName mPath >>= return . fromMaybe (fail "Failed to parse URI")
	let resources = mkResTree [
		([], emptyResource { resGet = Just $ pathResource metaJson, resIsGreedy = True })
		]
	runHttpd config resources []