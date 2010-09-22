import           Control.Monad
import qualified Data.Map                  as Map
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BUL
import           Data.List
import           Data.Maybe
import           Hellnet
import           Hellnet.Crypto
import           Hellnet.Files
import           Hellnet.FileTree
import           Hellnet.Meta
import           Hellnet.Network
import           Hellnet.Storage
import           Hellnet.URI
import           Hellnet.Utils
import           Safe
import           System.Console.GetOpt
import           System.Environment                (getArgs)
import           System.FilePath
import           System.FilePath.Glob      as Glob
import           System.Directory
import           System.Time
import qualified System.Directory.Tree     as Tree
import           Text.HJson                as JSON
import           Text.JSON.JPath
import           Text.Printf

data Opts = Opts {
	encrypt :: Bool,
	encKey :: Maybe Key,
	updateMeta :: Bool,
	encryptMeta :: Bool,
	metaEncKey :: Maybe Key,
	verbose :: Bool,
	dryRun :: Bool,
	indexChunks :: Bool,
	forceInsert :: Bool
	}

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['m'] ["meta-encryption"]
		(OptArg (\s o ->  o {encryptMeta = True, metaEncKey = maybe (Nothing) (Just . hexToHash) s}) "key") "Use encryption on meta (optionally (if encrypting) with specified key)",
	Option ['e'] ["encrypt"]
		(OptArg (\s o ->  o {encrypt = True, encKey = maybe (Nothing) (Just . hexToHash) s}) "key") "Encrypt content (optionally with specified key)",
	Option ['u'] ["update-meta"]
		(NoArg (\o -> o {updateMeta = True})) "Automatically update meta before retrieval",
	Option ['v'] ["verbose"]
		(NoArg (\o -> o {verbose = True})) "Be verbose",
	Option ['d'] ["dry-run"]
		(NoArg (\o -> o {dryRun = True})) "Do not actually touch anything",
	Option ['i'] ["index"]
		(NoArg (\o -> o {indexChunks = True})) "Index files instead of inserting",
	Option ['f'] ["forec"]
		(NoArg (\o -> o {forceInsert = True})) "Force updating files"
	]

defaultOptions = Opts {
	encrypt = False,
	encKey = Nothing,
	updateMeta = False,
	encryptMeta = False,
	metaEncKey = Nothing,
	verbose = False,
	dryRun = False,
	indexChunks = False,
	forceInsert = False
	}

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

pullTreeWalker ignores opts cpath (Dir rmtime rfiles) (Just (File _ _)) = do
	printf "File %s exists, but it should be a directory; deleting\n" $ joinPath cpath
	when (not $ dryRun opts) $ removeFile $ joinPath cpath
	pullTreeWalker ignores opts cpath (Dir rmtime rfiles) Nothing
pullTreeWalker ignores opts cpath (Dir rmtime rfiles) (Just (Dir lmtime lfiles)) = do
	when (verbose opts) $ printf "Traversing directory %s\n" $ joinPath cpath
	mapM_ (\(name, tree) -> pullTreeWalker ignores opts (cpath ++ [name]) tree $ Map.lookup name lfiles) $ Map.toList rfiles
pullTreeWalker ignores opts cpath (Dir rmtime rfiles) Nothing = do
	printf "Creating directory %s\n" $ joinPath cpath
	when (not $ dryRun opts) $ createDirectoryIfMissing True $ joinPath cpath
	mapM_ (\(name, tree) -> pullTreeWalker ignores opts (cpath ++ [name]) tree Nothing) $ Map.toList rfiles
-- File cases
pullTreeWalker ignores opts cpath (File rmtime link) (Just (Dir lmtime lfiles)) = do
	printf "Directory %s exists, but it should be a file instead;  rm-r'ing\n" $ joinPath cpath
	when (not $ dryRun opts) $ removeDirectoryRecursive $ joinPath cpath
	pullTreeWalker ignores opts cpath (File rmtime link) Nothing
pullTreeWalker ignores opts cpath (File rmtime link) (Just (File lmtime llink)) = do
	if rmtime == lmtime && not (forceInsert opts) then do
		when (verbose opts) $ printf "File %s is as new as remote one; skipping\n" $ joinPath cpath
		return ()
		else do
		when (not $ dryRun opts) $ removeFile $ joinPath cpath
		pullTreeWalker ignores opts cpath (File rmtime link) Nothing
pullTreeWalker ignores opts cpath (File rmtime link) Nothing = do
	printf "Updating file %s\n" $ joinPath cpath
	mBsl <- findURI $ fromMaybe (error "bad link") $ parseHellnetURI link
	case mBsl of
		Nothing -> error ("URI " ++ link ++ " not found!")
		Just conts -> when (not $ dryRun opts) $ BSL.writeFile (joinPath cpath) conts

pushTreeWalker ignores opts cpath (Dir lmtime lfiles) (Just (File _ _)) = do
	printf "File %s exists, but it should be a directory; deleting\n" $ joinPath cpath
	removeFile $ joinPath cpath
	pushTreeWalker ignores opts cpath (Dir lmtime lfiles) Nothing
pushTreeWalker ignores opts cpath (Dir lmtime lfiles) (Just (Dir rmtime rfiles)) = do
	when (verbose opts) $ printf "Traversing directory %s\n" $ joinPath cpath
	lfiles' <- mapM (\(name, tree) -> do
		let path' = cpath ++ [name]
		let ignoredpath = zipWith ($) (map (Glob.match) ignores) $ repeat $ joinPath $ tail path'
		let ignoredfile = zipWith ($) (map (Glob.match) ignores) $ repeat name
		if or (ignoredpath ++ ignoredfile) then do
			when (verbose opts) $ printf "Omitting entry %s\n" $ joinPath path'
			return Nothing
			else do
			tree' <- pushTreeWalker ignores opts (path') tree $ Map.lookup name rfiles
			return $ Just (name, tree')
		) $ Map.toList lfiles
	return $ Dir lmtime $ Map.fromList $ catMaybes lfiles'
pushTreeWalker ignores opts cpath (Dir lmtime lfiles) Nothing = do
	printf "Creating directory %s\n" $ joinPath cpath
	lfiles' <- mapM (\(name, tree) -> do
		let path' = cpath ++ [name]
		let ignoredpath = zipWith ($) (map (Glob.match) ignores) $ repeat $ joinPath $ tail path'
		let ignoredfile = zipWith ($) (map (Glob.match) ignores) $ repeat name
		if or (ignoredpath ++ ignoredfile) then do
			when (verbose opts) $ printf "Omitting entry %s\n" $ joinPath path'
			return Nothing
			else do
			tree' <- pushTreeWalker ignores opts (cpath ++ [name]) tree Nothing
			return $ Just (name, tree')
		) $ Map.toList $ lfiles
	return $ Dir lmtime $ Map.fromList $ catMaybes lfiles'
-- File cases
pushTreeWalker ignores opts cpath (File lmtime link) (Just (File rmtime rlink)) = do
	if rmtime == lmtime  && not (forceInsert opts) then do
		when (verbose opts) $ printf "File %s is as new as local one; skipping\n" $ joinPath cpath
		return $ File rmtime rlink
		else do
		pushTreeWalker ignores opts cpath (File lmtime link) Nothing
pushTreeWalker ignores opts cpath (File lmtime _) _ = do
	printf "Updating file %s\n" $ joinPath cpath
	url <- if indexChunks opts then
		indexData Nothing (joinPath cpath)
		else
		insertData Nothing =<< BSL.readFile (joinPath cpath)
	return (File lmtime $ show url)

getIgnores dir = do
	let fpath = joinPath [dir, ".helldirignore"]
	exists <- doesFileExist fpath
	if exists then do
		cont <- readFile fpath
		return $ map (Glob.compile) $ lines cont
		else
		return []

main = do
	argz <- getArgs
	let (optz, args, errs) = getOpt Permute options argz
	let preOpts = processOptions defaultOptions optz
	let (args', opts) = case (length args > 2, parseHellnetURI (args !! 2)) of
		(True, Just (MetaURI keyid mnameM mpath encKey _)) -> do
			let
				namepath =	case (mnameM, mpath) of
					(Just mname, []) -> [mname]
					(Just mname, mp) -> [mname, mp]
					otherwise -> [];
				opts' = case encKey of
					Just k -> preOpts {encryptMeta = True, metaEncKey = Just k}
					otherwise -> preOpts
					in (take 2 args ++ hashToHex keyid : namepath, opts')
		otherwise -> (args, preOpts)
	theKey <- if encrypt opts then
		(return . Just) =<< (maybe (genKey) (return) $ encKey opts)
		else
		return Nothing
	theMetaKey <- if encryptMeta opts then
		(return . Just) =<< (maybe (genKey) (return) $ metaEncKey opts)
		else
		return Nothing
	let ensureSuppliedMetaKey = when (isNothing (metaEncKey opts) && encryptMeta opts) (fail "You can't decrypt with random key!")
	let announceMetaKey = when (isNothing (metaEncKey opts) && encryptMeta opts) $ printf "Your meta key will be %s" (hashToHex $ fromMaybe (error "Meta key is going to be used but wasn't generated") theMetaKey)
	case args' of
		["pull", dirName, metaKey, mName] -> do
			keyid <- resolveKeyName metaKey
			ignores <- getIgnores dirName
			when (updateMeta opts) (fetchMeta keyid mName >> return ())
			ensureSuppliedMetaKey
			putStrLn "Synchronizing local tree with remote"
			contM <- findMetaContentByName theMetaKey keyid mName ""
			let remoteTree = fromMaybe (error "Couldn't parse tree") $ fromJson $ fromMaybe (error "Couldn't get content") $ headMay $ fromMaybe (error "Couldn't get content") $ contM 
			putStrLn "Reading directory tree..."
			dirTree <- Tree.readDirectoryWith (const $ return ()) dirName
			currentTreeM <- convertTree [dirName] $ Tree.free dirTree
			let currentTree = fromMaybe (error "Couldn't traverse local tree") currentTreeM
			
			currentTree' <- (pullTreeWalker ignores opts [dirName] remoteTree $ Just currentTree)
			putStrLn "All done"
			announceMetaKey
		["push", dirName, metaKey, mName] -> do
			keyid <- resolveKeyName metaKey
			when (updateMeta opts) (fetchMeta keyid mName >> return ())
			ignores <- getIgnores dirName
			putStrLn "Synchronizing remote tree with local"
			metaM <- findMeta keyid mName
			when (isJust metaM) (ensureSuppliedMetaKey)
			when (isNothing metaM) $ putStrLn "Warning: Meta not found. Creating new one."
			let meta = fromMaybe (emptyMeta {keyID = keyid, metaName = mName}) metaM
			contM <- findMetaContent theMetaKey meta ""
			let remoteTree = case contM of
				Nothing -> Nothing
				Just [] -> Nothing
				Just (js:_) -> fromJson js
		
			putStrLn "Reading directory tree..."
			dirTree <- Tree.readDirectoryWith (const $ return ()) dirName
			currentTreeM <- convertTree [] $ Tree.free dirTree
			let currentTree = fromMaybe (error "Couldn't traverse local tree") currentTreeM
			
			putStrLn "Updating index..."
			remoteTree' <- pushTreeWalker ignores opts [dirName] currentTree remoteTree
			when (not $ dryRun opts) $ do
				putStrLn "Updating meta..."
				link' <- insertData theKey $ maybe (id) (encryptSym) theMetaKey $ BUL.fromString $ JSON.toString $ toJson remoteTree'
				newMetaM <- regenMeta $ meta {contentURI = link'}
				case newMetaM of
					Nothing -> error "Failed to sign meta"
					Just newMeta -> do
						storeMeta newMeta
						putStrLn "Success"
			announceMetaKey
		otherwise -> do
			let usageStrings =  [
				"push <path> <key id> <meta name>    -- Update meta to match file structure in <path>",
				"pull <path> <key id> <meta name>    -- Update file structure to match one in meta"
				] in error $ usageInfo (intercalate "\n" $ map ("hell-dir "++) usageStrings) options ++ concat errs