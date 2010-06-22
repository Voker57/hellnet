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

main = do
	argz <- getArgs
	let (optz, args, errs) = getOpt Permute options argz
	let opts = processOptions defaultOptions optz
	let [action, dirName, metaKey, mName] = args
	keyid <- resolveKeyName metaKey
	case action of
		"pull" -> do
			cont <- findMetaContentByName keyid mName
			let remoteTree = fromMaybe (error "Could not read file tree from JSON") $ fromJson $ fromMaybe (error "JSON parsing failed") cont
			
			dirTree <- Tree.readDirectoryWith (const $ return ()) dirName
			currentTree <- convertTree [dirName] $ Tree.free dirTree
			
			let treeWalker cpath (Dir rmtime rfiles) (Just (File _ _)) = do
				removeFile cpath
				treeWalker cpath (Dir rmtime rfiles) Nothing
			let treeWalker cpath (Dir rmtime rfiles) (Just (Dir lmtime lfiles)) = do
				if rmtime <= lmtime then
					return ()
					else
					mapM_ (\(name, tree) -> treeWalker (joinPath [cpath, name]) tree $ Map.lookup name lfiles) $ Map.toList rfiles
			let treeWalker cpath (Dir rmtime rfiles) Nothing = do
				createDirectoryIfMissing True cpath
				mapM_ (\(name, tree) -> treeWalker (joinPath [cpath, name]) tree Nothing) $ Map.toList rfiles
			-- File cases
			let treeWalker cpath (File rmtime link) (Just (Dir lmtime lfiles)) = do
				removeDirectoryRecursive cpath
				treeWalker cpath (File rmtime link) Nothing
			let treeWalker cpath (File rmtime link) (Just (File lmtime llink)) = do
				if rmtime <= lmtime then
					return ()
					else do
					removeFile cpath
					treeWalker cpath (File rmtime link) Nothing
			let treeWalker cpath (File rmtime link) Nothing = do
				mBsl <- findURI $ fromMaybe (error "bad link") $ parseHellnetURI link
				case mBsl of
					Nothing -> error ("URI " ++ link ++ " not found!")
					Just conts -> BSL.writeFile cpath conts
			
			treeWalker dirName remoteTree $ Just currentTree
		"push" -> do
			metaM <- findMeta keyid mName
			let meta = fromMaybe (emptyMeta {keyID = keyid, metaName = mName}) metaM
			contM <- findMetaContent meta
			let remoteTree = (fromMaybe (Nothing) $ fmap (fromJson) contM) :: Maybe FileTree
			
			dirTree <- Tree.readDirectoryWith (const $ return ()) dirName
			currentTree <- convertTree [dirName] $ Tree.free dirTree
			
			let treeWalker cpath (Dir lmtime lfiles) (Just (File _ _)) = do
				removeFile cpath
				treeWalker cpath (Dir lmtime lfiles) Nothing
			let treeWalker cpath (Dir lmtime lfiles) (Just (Dir rmtime rfiles)) = do
				if rmtime >= lmtime then
					return $ Dir rmtime rfiles
					else do
					lfiles' <- mapM (\(name, tree) -> do
						tree' <- treeWalker (joinPath [cpath, name]) tree $ Map.lookup name rfiles
						return (name, tree')) $ Map.toList lfiles
					return $ Dir lmtime $ Map.fromList lfiles'
			let treeWalker cpath (Dir lmtime lfiles) Nothing = do
				lfiles' <- mapM (\(key, value) -> do
					tree' <- treeWalker (joinPath [cpath, key]) value Nothing
					return (key, tree')) $ Map.toList $ lfiles
				return $ Dir lmtime $ Map.fromList lfiles'
			-- File cases
			let treeWalker cpath (File lmtime link) (Just (File rmtime rlink)) = do
				if rmtime >= lmtime then
					return $ File rmtime rlink
					else
					treeWalker cpath (File lmtime link) Nothing
			let treeWalker cpath (File lmtime _) _ = do
				url <- insertData Nothing =<< BSL.readFile cpath
				return (File lmtime $ show url)
			
			print =<< treeWalker dirName (fromMaybe (error "Couldn't traverse local tree") currentTree) remoteTree