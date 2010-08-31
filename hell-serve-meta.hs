import Network.HTTP.Lucu as Lucu
import Hellnet.Network
import Hellnet.URI
import Control.Monad.Trans
import Text.JSON.JPath
import Text.HJson
import System.Environment
import Data.Maybe

pathResource metaJson = do
	path <- getPathInfo
	let currentJPath = map (ObjectLookup) path
	case jPath currentJPath metaJson of
		[] -> setStatus NotFound
		[JString u] -> do
			result <- liftIO $ findURI (fromMaybe (error "Failed to parse URI") $ parseHellnetURI u)
			case result of
				Nothing -> setStatus NotFound
				Just d -> do
					setContentType $ MIMEType "application" "octet-stream" []
					outputLBS d
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