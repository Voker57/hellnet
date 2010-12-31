import System.Console.GetOpt
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BUL
import Hellnet
import Hellnet.Crypto
import Hellnet.ExternalChunks
import Hellnet.Meta
import Hellnet.Network
import Hellnet.Storage
import Hellnet.Utils
import Data.Char
import Data.Maybe
import Data.Digest.SHA512 as SHA512
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.HJson as JSON
import Text.Printf

data Opts = Opts {
	fixErrors :: Bool
	, checkStore :: Bool
	, checkExternal :: Bool
	, checkMeta :: Bool
	}

defaultOptions = Opts {
	fixErrors = False
	, checkStore = True
	, checkExternal = True
	, checkMeta = True
	}

disableAllChecks o = o {
	checkStore = False
	, checkExternal = False
	, checkMeta = False
	}

options = [
	Option ['f'] ["fix-errors"]
		(NoArg (\o -> o {fixErrors = True})) "Fix errors",
	Option ['s'] ["store"]
		(NoArg (\o -> (disableAllChecks o) {checkStore = True})) "Check only store",
	Option ['e'] ["external"]
		(NoArg (\o -> (disableAllChecks o) {checkExternal = True})) "Check only external chunks",
	Option ['m'] ["meta"]
		(NoArg (\o -> (disableAllChecks o) {checkMeta = True})) "Check only metas"
	]

filterDots = return . filter (not . (`elem` [".", ".."]))

nprintfn s = printf (concat ["\n", s, "\n"])

checkHashesInDirectory opts d = do
	putStr $ d ++ "..."
	dfp <- (toFullPath $ "store" </> d)
	dirConts <- getDirectoryContents dfp >>= filterDots
	mapM (checkChunk opts) $ zip (repeat d) dirConts

checkChunk opts (d, c) = do
	let hsh = decrockford (d++c)
	fp <- toFullPath $ joinPath ["store", d, c]
	datM <- getFile fp
	case datM of
		Just dat -> if BSL.length dat > fromInteger chunkSize then do
			nprintfn "Chunk %s%s is too big, removing" d c
			when (fixErrors opts) $ removeFile fp
			else if Hellnet.Crypto.hash dat == hsh then
				return ()
				else do
				nprintfn "Hash mismatch in %s%s, removing" d c
				when (fixErrors opts) $ removeFile fp
		Nothing -> return ()

checkMappingsInDirectory opts d = do 
	putStr $ d ++ "..."
	dfp <- (toFullPath $ "chunkmap" </> d)
	dirConts <- getDirectoryContents dfp >>= filterDots
	mapM (checkMapping opts) $ zip (repeat d) dirConts
	
checkMapping opts (d, c) = do
	let hsh = decrockford (d++c)
	fp <- toFullPath $ joinPath ["chunkmap", d, c]
	datM <- getFile fp
	case datM of
		Just dat -> case fmap (fromJson) $ JSON.fromString $ BUL.toString dat of
			Right (Just fl) -> do
				ch <- getExternalChunk fl
				case ch of
					Just ch -> do
						when (Hellnet.Crypto.hash ch /= hsh) $ do
							nprintfn "Hash mismatch in %s%s, removing" d c
							when (fixErrors opts) $ removeFile fp
					Nothing -> do
						nprintfn "Failed to fetch external chunk %s%s, removing" d c
						when (fixErrors opts) $ removeFile fp
			otherwise -> do
				nprintfn "Failed to read external chunk reference %s%s, removing" d c
				when (fixErrors opts) $ removeFile fp
		Nothing -> return ()

checkMetasInDirectory opts d = do
	putStr $ d ++ "..."
	dfp <- (toFullPath $ "meta" </> d)
	dirConts <- getDirectoryContents dfp >>= filterDots
	mapM (checkMetas opts) $ zip (repeat d) dirConts

checkMetas opts (d, c) = do
	fp <- toFullPath $ joinPath ["meta", d, c]
	datM <- getFile fp
	case datM of
		Just dat -> do
			case Hellnet.Meta.fromByteString dat of
				Just meta -> do
					result <- verifyMeta meta
					case result of
						Just True -> return ()
						Just False -> do
							nprintfn "Meta %s/%s signature check failed, removing" d c
							when (fixErrors opts) $ removeFile fp
						Nothing -> do
							nprintfn "Failed to get key for %s, skipping" d
							return ()
				Nothing -> do
					printf "Couldn't parse meta %s/%s, removing" d c
					when (fixErrors opts) $ removeFile fp
		Nothing -> return ()

main = do
	hSetBuffering stdout NoBuffering
	args <- getArgs
	let (optz, argz, errs) = getOpt Permute options args
	let opts = processOptions defaultOptions optz
	when (checkStore opts) $ do 
		putStr "Checking store: "
		storePath <- toFullPath "store"
		storeConts <- getDirectoryContents storePath >>= filterDots
		mapM (checkHashesInDirectory opts) $ sort storeConts
		putStrLn "done"
	when (checkExternal opts) $ do 
		putStr "Checking external chunks: "
		chunkMapPath <- toFullPath "chunkmap"
		cmConts <- getDirectoryContents chunkMapPath >>= filterDots
		mapM (checkMappingsInDirectory opts) $ sort cmConts
		putStrLn "done"
	when (checkMeta opts) $ do
		putStr "Checking meta: "
		metasPath <- toFullPath "meta"
		mConts <- getDirectoryContents metasPath >>= filterDots
		mapM (checkMetasInDirectory opts) $ sort mConts
		putStrLn "done"