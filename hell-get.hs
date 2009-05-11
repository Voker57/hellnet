import Hellnet.Storage
import Hellnet.Utils
import Hellnet.Network
import System.Environment (getArgs)
import Data.Char (chr)
import Data.ByteString as BS hiding (length, head)
import Data.ByteString.Char8 as BS8 hiding (length, head)
import System.IO.Error
import Data.Either

main = do
	args <- getArgs
	if (or [((length args) < 2), ((length (args !! 1)) /= 128)]) then do
			Prelude.putStrLn "Usage: hell-get {file,chunk} <hash>"
		else do
			let hsh = hexToHash (args !! 1)
			if (head args) == "chunk" then do
				localConts <- try (getChunk hsh)
				let getConts = either (const (findChunk hsh)) (return . Just) localConts
				conts <- getConts
				maybe (error "Chunk not found in network") (BS.putStr . BS.pack) conts
				else
				print "wtf"