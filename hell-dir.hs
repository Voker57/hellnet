import qualified Data.Map as Map
import Data.Maybe
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath
import System.Directory
import System.Time
import qualified System.Directory.Tree as Tree
import Hellnet.Files
import Hellnet.Utils
import Text.HJson
import Text.JSON.JPath

data FileTree = Dir Integer (Map.Map FilePath FileTree) | File Integer String deriving (Eq, Show)

instance Jsonable FileTree where
	fromJson j = case jPath "type" j of
		[JString "file"] -> case (jPath "modified" j, jPath "link" j) of
			([JNumber modTimestamp], [JString link]) -> Just $ File (round modTimestamp) link
			otherwise -> Nothing
		[JString "dir"] -> case (jPath "modified" j, jPath "contents" j) of
			([JNumber modTimestamp], [conts@(JObject _)]) -> case fromJson conts of
				Nothing -> Nothing
				Just mp -> Just $ Dir (round modTimestamp) mp
			otherwise -> Nothing
	toJson (File md link) = JObject $ Map.fromList [
		("modified", toJson md),
		("link", toJson link)
		]
		
data Opts = Opts { recursive :: Bool }

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['r'] ["recursive"]
		(NoArg (\o -> o {recursive = True}) ) "Recurse into subdirectories"
	]

defaultOptions = Opts { recursive = False }

convertTree :: [FilePath] -> Tree.DirTree a -> IO (Maybe FileTree)
convertTree fs d@(Tree.Dir{}) = do
	let fs' = fs ++ [Tree.name d]
	modTime <- getModificationTime $ joinPath (fs')
	let TOD i _ = modTime
	let treeToPair d = do
		convM <- convertTree fs' d
		return $ fmap (\conv -> (Tree.name d, conv)) convM
	conts <- mapM (treeToPair) (Tree.contents d) >>= return . catMaybes
	return $ Just $ Dir i $ Map.fromList conts
convertTree fs d@(Tree.File{}) = do
	modTime <- getModificationTime $ joinPath (fs ++ [Tree.name d])
	let TOD i _ = modTime
	return $ Just $ File i ""
convertTree _ _ = return Nothing

main = do
	argz <- getArgs
	let (optz, args, errs) = getOpt Permute options argz
	let opts = processOptions defaultOptions optz
	let dirName = head args
	dirTree <- Tree.readDirectoryWith (const $ return ()) dirName
	mTree <- convertTree [dirName] $ Tree.free dirTree
	print mTree