import Network.HTTP.Lucu as Lucu
import Network
import Hellnet.Storage (toFullPath)
import System.Environment

main = do
	args <- getArgs
	let port = if length args == 0 then
		6666
		else
		fromIntegral (read (head args) :: Int) :: PortNumber
	let config = defaultConfig { cnfServerPort = PortNumber port };
	chunksPath <- toFullPath "store"
	let resources = mkResTree [ (["chunks"], staticDir chunksPath) ]
	runHttpd config resources []