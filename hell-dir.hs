import           Control.Monad
import qualified Data.Map                  as Map
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BUL
import           Data.Maybe
import           Hellnet.Files
import           Hellnet.Meta
import           Hellnet.Network
import           Hellnet.Storage
import           Hellnet.URI
import           Hellnet.Utils
import           System.Console.GetOpt
import           System.Environment                (getArgs)
import           System.FilePath
import           System.Directory
import           System.Time
import qualified System.Directory.Tree     as Tree
import           Text.HJson                as JSON
import           Text.JSON.JPath
import           Text.Printf

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
		("type", toJson "file"),
		("modified", toJson md),
		("link", toJson link)
		]
	toJson (Dir md conts) = JObject $ Map.fromList [
		("type", toJson "dir"),
		("modified", toJson md),
		("contents", toJson conts)
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

pullTreeWalker cpath (Dir rmtime rfiles) (Just (File _ _)) = do
	printf "File %s exists, but it should be a directory; deleting\n" cpath
	removeFile cpath
	pullTreeWalker cpath (Dir rmtime rfiles) Nothing
pullTreeWalker cpath (Dir rmtime rfiles) (Just (Dir lmtime lfiles)) = do
	if rmtime <= lmtime then do
		printf "Directory %s is newer then remote one; skipping\n" cpath
		return ()
		else do
		printf "Directory %s needs updating\n" cpath
		mapM_ (\(name, tree) -> pullTreeWalker (joinPath [cpath, name]) tree $ Map.lookup name lfiles) $ Map.toList rfiles
pullTreeWalker cpath (Dir rmtime rfiles) Nothing = do
	printf "Creating directory %s\n" cpath
	createDirectoryIfMissing True cpath
	mapM_ (\(name, tree) -> pullTreeWalker (joinPath [cpath, name]) tree Nothing) $ Map.toList rfiles
-- File cases
pullTreeWalker cpath (File rmtime link) (Just (Dir lmtime lfiles)) = do
	printf "Directory %s exists, but it should be a file instead;  rm-r'ing" cpath
	removeDirectoryRecursive cpath
	pullTreeWalker cpath (File rmtime link) Nothing
pullTreeWalker cpath (File rmtime link) (Just (File lmtime llink)) = do
	if rmtime <= lmtime then do
		printf "File %s is newer then remote one; skipping\n" cpath
		return ()
		else do
		printf "File %s needs updating\n" cpath
		removeFile cpath
		pullTreeWalker cpath (File rmtime link) Nothing
pullTreeWalker cpath (File rmtime link) Nothing = do
	printf "Updating file %s\n" cpath
	mBsl <- findURI $ fromMaybe (error "bad link") $ parseHellnetURI link
	case mBsl of
		Nothing -> error ("URI " ++ link ++ " not found!")
		Just conts -> BSL.writeFile cpath conts

pushTreeWalker cpath (Dir lmtime lfiles) (Just (File _ _)) = do
	printf "File %s exists, but it should be a directory; deleting\n" cpath
	removeFile cpath
	pushTreeWalker cpath (Dir lmtime lfiles) Nothing
pushTreeWalker cpath (Dir lmtime lfiles) (Just (Dir rmtime rfiles)) = do
	if rmtime >= lmtime then do
		printf "Directory %s is newer then remote one; skipping\n" cpath
		return $ Dir rmtime rfiles
		else do
		printf "Directory %s needs updating\n" cpath
		lfiles' <- mapM (\(name, tree) -> do
			tree' <- pushTreeWalker (joinPath [cpath, name]) tree $ Map.lookup name rfiles
			return (name, tree')) $ Map.toList lfiles
		return $ Dir lmtime $ Map.fromList lfiles'
pushTreeWalker cpath (Dir lmtime lfiles) Nothing = do
	printf "Creating directory %s\n" cpath
	lfiles' <- mapM (\(key, value) -> do
		tree' <- pushTreeWalker (joinPath [cpath, key]) value Nothing
		return (key, tree')) $ Map.toList $ lfiles
	return $ Dir lmtime $ Map.fromList lfiles'
-- File cases
pushTreeWalker cpath (File lmtime link) (Just (File rmtime rlink)) = do
	if rmtime >= lmtime then do
		printf "File %s is newer then remote one; skipping\n" cpath
		return $ File rmtime rlink
		else do
		printf "File %s needs updating\n" cpath
		pushTreeWalker cpath (File lmtime link) Nothing
pushTreeWalker cpath (File lmtime _) _ = do
	printf "Updating file %s\n" cpath
	url <- insertData Nothing =<< BSL.readFile cpath
	return (File lmtime $ show url)

main = do
	argz <- getArgs
	let (optz, args, errs) = getOpt Permute options argz
	let opts = processOptions defaultOptions optz
	let [action, dirName, metaKey, mName] = args
	keyid <- resolveKeyName metaKey
	case action of
		"pull" -> do
			putStrLn "Synchronizing local tree with remote"
			contM <- findMetaContentByName keyid mName
			let remoteTree = case contM of
				Just cont -> fromMaybe (error "Couldn't load file tree") $ fromJson cont
				Nothing -> error "Couldn't parse JSON"
			putStrLn "Reading directory tree..."
			dirTree <- Tree.readDirectoryWith (const $ return ()) dirName
			currentTreeM <- convertTree [dirName] $ Tree.free dirTree
			let currentTree = fromMaybe (error "Couldn't traverse local tree") currentTreeM
			
			print =<< (pullTreeWalker dirName remoteTree $ Just currentTree)
		"push" -> do
			putStrLn "Synchronizing remote tree with local"
			metaM <- findMeta keyid mName
			when (isNothing metaM) $ putStrLn "Warning: Meta not found. Creating new one."
			let meta = fromMaybe (emptyMeta {keyID = keyid, metaName = mName}) metaM
			contM <- findMetaContent meta
			let remoteTree = (fromMaybe (Nothing) $ fmap (fromJson) contM) :: Maybe FileTree
			
			putStrLn "Reading directory tree..."
			dirTree <- Tree.readDirectoryWith (const $ return ()) dirName
			currentTreeM <- convertTree [] $ Tree.free dirTree
			let currentTree = fromMaybe (error "Couldn't traverse local tree") currentTreeM
			
			print =<< pushTreeWalker dirName currentTree remoteTree