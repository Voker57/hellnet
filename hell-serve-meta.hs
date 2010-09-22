import Hellnet.ArchdaemonModules
import Archdaemon
import Network.HTTP.Lucu
import Hellnet.Network
import Data.Maybe
import Hellnet.URI
import Hellnet.Utils
import System.Environment
import System.Console.GetOpt

data Opts = Opts {
	bindTo :: Maybe String,
	bind6To :: Maybe String
	}

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['b'] ["bind"]
		(ReqArg (\s o ->  o {bindTo = Just s}) "host") "Bind to that IPv4 address",
	Option ['6'] ["bind6"]
		(ReqArg (\s o ->  o {bind6To = Just s}) "host") "Bind to that IPv6 address"
	]

defaultOptions = Opts {
	bindTo = Nothing,
	bind6To = Nothing
	}


main = do
	argz <- getArgs
	let (optz, args, errs) = getOpt Permute options argz
	let opts = processOptions defaultOptions optz
	case args of
		[port, meta] -> do
			let config = defaultConfig { cnfServerPort = port, cnfServerV4Addr = bindTo opts, cnfServerV6Addr = bind6To opts }
			let Just (MetaURI hsh (Just mName) mPath mKey _) = parseHellnetURI meta
			[metaJson] <- findMetaContentByName mKey hsh mName mPath >>= return . fromMaybe (fail "Failed to parse URI")
			Archdaemon.launch config [
				([], metaModule metaJson)
				]
		otherwise -> putStrLn $ (usageInfo "Usage: hell-serve-meta <port> <meta URI>" options) ++ concat errs