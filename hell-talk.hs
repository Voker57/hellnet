import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Hellnet
import Hellnet.Network
import Hellnet.Meta
import Hellnet.Storage
import System.Environment

talkMeta = Meta "talk" "0"

prettyPrintMessage :: String -> IO ()
prettyPrintMessage s = do
	putStrLn ""
	putStrLn s
	putStrLn "-------------"

printLatestTalks :: Int -> IO ()
printLatestTalks n = do
	hs <- findHashesByMeta talkMeta
	msgs <- mapM (findChunk Nothing) (reverse $ take n $ reverse hs)
	mapM_ (prettyPrintMessage) $ map (BS8.unpack) $ catMaybes msgs

main = do
	args <- getArgs
	if null args then
		printLatestTalks 10
		else do
			putStrLn "Please input message:"
			text <- BS.getContents
			hash <- insertChunk Nothing (BS.unpack text)
			addHashesToMeta talkMeta [hash]
			putStrLn "Posted!"