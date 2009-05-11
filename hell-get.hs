import Hellnet.Storage
import Hellnet.Utils
import System.Environment (getArgs)
import Data.Char (chr)
import Data.ByteString as BS hiding (length, head)
import Data.ByteString.Char8 as BS8 hiding (length, head)
import System.IO.Error
import Data.Either

getRemoteConts _ = undefined

main = do
	args <- getArgs
	if (or [((length args) == 0), ((length (head args)) /= 128)]) then do
			Prelude.putStrLn "Usage: hell-get <hash>"
		else do
			localConts <- try (getFileContents (hexToHash (head args)))
			let getConts = either (getRemoteConts) (return) localConts
			conts <- getConts
			BS.putStr (BS.pack conts)