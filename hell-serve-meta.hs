import Hellnet.ArchdaemonModules
import Archdaemon
import Network.HTTP.Lucu
import Hellnet.Network
import Data.Maybe
import Hellnet.URI
import System.Environment

main = do
	args <- getArgs
	let port = head args
	let config = defaultConfig { cnfServerPort = port }
	let Just (MetaURI hsh (Just mName) mPath mKey _) = parseHellnetURI $ args !! 1
	[metaJson] <- findMetaContentByName mKey hsh mName mPath >>= return . fromMaybe (fail "Failed to parse URI")
	Archdaemon.launch config [
		([], metaModule metaJson)
		]