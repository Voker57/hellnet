import System.Console.GetOpt
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BUL
import Hellnet
import Hellnet.Crypto
import Hellnet.ExternalChunks
import Hellnet.Storage
import Data.Char
import Data.Maybe
import Hellnet.Utils
import Data.Digest.SHA512 as SHA512
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.HJson as JSON

data Opts = Opts {
	fixErrors :: Bool
	, checkStore :: Bool
	, checkExternal :: Bool
	}

defaultOptions = Opts {
	fixErrors = False
	, checkStore = True
	, checkExternal = True
	}

options = [
	Option ['f'] ["fix-errors"]
		(NoArg (\o -> o {fixErrors = True})) "Fix errors",
	Option ['s'] ["store"]
		(NoArg (\o -> o {checkStore = True, checkExternal = False})) "Check only store",
	Option ['e'] ["external"]
		(NoArg (\o -> o {checkStore = False, checkExternal = True})) "Check only external chunks"
	]

filterDots = return . filter (all (isAlphaNum))

checkHashesInDirectory opts d = do
	putStr $ d ++ "..."
	dfp <- (toFullPath $ "store" </> d)
	dirConts <- getDirectoryContents dfp >>= filterDots
	mapM (checkChunk opts) $ zip (repeat d) dirConts

checkChunk opts (d, c) = do
	let hsh = hexToHash (d++c)
	fp <- toFullPath $ joinPath ["store", d, c]
	dat <- getFile fp >>= return . fromJust
	if BSL.length dat > fromInteger chunkSize then do
		putStrLn $ "\nChunk " ++ d ++ c ++" is too big, removing"
		when (fixErrors opts) $ removeFile fp
		else if Hellnet.Crypto.hash dat == hsh then
			return ()
			else do
			putStrLn $ "\nHash mismatch in " ++ d ++ c ++ ", removing"
			when (fixErrors opts) $ removeFile fp

checkMappingsInDirectory opts d = do 
	putStr $ d ++ "..."
	dfp <- (toFullPath $ "chunkmap" </> d)
	dirConts <- getDirectoryContents dfp >>= filterDots
	mapM (checkMapping opts) $ zip (repeat d) dirConts
	
checkMapping opts (d, c) = do
	let hsh = hexToHash (d++c)
	fp <- toFullPath $ joinPath ["chunkmap", d, c]
	dat <- getFile fp >>= return . fromJust
	case fmap (fromJson) $ JSON.fromString $ BUL.toString dat of
		Right (Just fl) -> do
			ch <- getExternalChunk fl
			case ch of
				Just ch -> do
					when (Hellnet.Crypto.hash ch /= hsh) $ do
						putStrLn $ "\nHash mismatch in " ++ d ++ c ++ ", removing"
						when (fixErrors opts) $ removeFile fp
				otherwise -> do
					putStrLn $ "Failed to fetch external chunk " ++ d ++ c ++ ", removing"
					when (fixErrors opts) $ removeFile fp
		otherwise -> do
			putStrLn $ "Failed to read external chunk reference " ++ d ++ c ++ ", removing"
			when (fixErrors opts) $ removeFile fp
	
main = do
	hSetBuffering stdout NoBuffering
	args <- getArgs
	let (optz, argz, errs) = getOpt Permute options args
	let opts = processOptions defaultOptions optz
	when (checkStore opts) $ do 
		putStrLn "Checking store: "
		storePath <- toFullPath "store"
		storeConts <- getDirectoryContents storePath >>= filterDots
		mapM (checkHashesInDirectory opts) $ sort storeConts
		putStrLn "done"
	when (checkExternal opts) $ do 
		putStrLn "Checking external chunks: "
		chunkMapPath <- toFullPath "chunkmap"
		cmConts <- getDirectoryContents chunkMapPath >>= filterDots
		mapM (checkMappingsInDirectory opts) $ sort cmConts
		putStrLn "done"
	