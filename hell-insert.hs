import Hellnet.Files
import Hellnet.Utils
import System.Environment (getArgs)
import Control.Monad

insertFilePrintHash fname = do
	hsh <- insertFile fname
	putStrLn (fname ++ ": " ++ (hashToHex hsh))

main = do
	args <- getArgs
	when (length args == 0) (putStrLn "Usage: hell-insert <file1> [<file2>...]")
	mapM (insertFilePrintHash) args
	putStr ""