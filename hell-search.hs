import Hellnet.Utils
import Hellnet.Network
import System.Environment

fetchThemPrintThem :: String -> IO ()
fetchThemPrintThem s = do
	let m = metaFromString s
	hs <- fetchHashesByMeta m
	mapM_ (\h -> putStrLn $ s ++ "\thell://file/" ++ (hashToHex h)) hs

main = do
	args <- getArgs
	if null args then
		putStrLn "Usage: hell-search <meta1> [<meta2> ...]"
		else
		mapM_ (fetchThemPrintThem) args