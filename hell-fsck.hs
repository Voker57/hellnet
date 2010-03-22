import qualified Data.ByteString as BS
import Hellnet
import Hellnet.Storage
import Data.Char
import Hellnet.Utils
import Data.Digest.SHA512 as SHA512
import Data.List
import System.Directory
import System.FilePath

filterDots = return . filter (all (isAlphaNum))

checkHashesInDirectory d = do
	putStr $ d ++ "..."
	dfp <- (toFullPath $ "store" </> d)
	dirConts <- getDirectoryContents dfp >>= filterDots
	mapM (checkChunk) $ zip (repeat d) dirConts

checkChunk (d, c) = do
	let hsh = hexToHash (d++c)
	fp <- toFullPath $ joinPath ["store", d, c]
	dat <- BS.readFile fp
	if BS.length dat > fromInteger chunkSize then do
		putStrLn $ "\nChunk " ++ d ++ c ++" is too big, removing"
		removeFile fp
		else if SHA512.hash (BS.unpack dat) == hsh then
			return ()
			else do
			putStrLn $ "\nHash mismatch in " ++ d ++ c ++ ", removing"
			removeFile fp

main = do
	putStrLn "Checking store: "
	storePath <- toFullPath "store"
	storeConts <- getDirectoryContents storePath >>= filterDots
	mapM (checkHashesInDirectory) $ sort storeConts
	putStrLn "done"