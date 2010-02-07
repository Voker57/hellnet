--------------------------------------------------------------------------------
--     This file is part of Hellnet
--
--     Hellnet is free software: you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation, either version 3 of the License, or
--     (at your option) any later version.
--
--     Hellnet is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
--
--     You should have received a copy of the GNU General Public License
--     along with Hellnet.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

import Control.Monad
import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.List
import Hellnet
import Hellnet.Meta as Meta
import Hellnet.Network
import Hellnet.Storage
import Hellnet.Utils
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Cmd
import Text.HJson as JSON

data Opts = Opts {
	encrypt :: Bool,
	encKey :: Maybe Key,
	updateMeta :: Bool
	}

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['e'] ["encrypt"]
		(OptArg (\s o ->  o {encrypt = True, encKey = maybe (Nothing) (Just . hexToHash) s}) "key") "Encrypt content (optionally with specified key)",
	Option ['u'] ["update-meta"]
		(NoArg (\o -> o {updateMeta = True})) "Automatically update meta before retrieval"
	]

defaultOptions = Opts {
	encrypt = False,
	encKey = Nothing,
	updateMeta = False
	}

fetchMetaPrintResult :: KeyID -> String -> IO ()
fetchMetaPrintResult keyid mname = do
	result <- fetchMeta keyid mname
	let metaName = hashToHex (take 10 keyid) ++ ".../" ++ mname
	putStrLn $ case result of
				True -> "Meta "++ metaName ++" updated"
				False -> "Meta "++ metaName ++" unchanged"

main = do
	argz <- getArgs
	keyAliases <- getKeyAliases
	let (optz, args, errs) = getOpt Permute options argz
	let opts = processOptions defaultOptions optz
	theKey <- if encrypt opts then
		(return . Just) =<< (maybe (genKey) (return) $ encKey opts)
		else
		return Nothing
	case args of
		["update", keyidHex, mname] -> do
			let keyid = hexToHash keyidHex
			fetchMetaPrintResult keyid mname
		["update", keyidHex] -> do
			let keyid = hexToHash keyidHex
			allmeta <- getMetaNames keyid
			mapM_ (fetchMetaPrintResult keyid) allmeta
		["get", keyidHex, mname, mpath] -> do
			let keyid = hexToHash keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			vs <- findMetaValue keyid mname mpath
			case vs of
				Nothing -> error "Meta not found"
				Just a -> mapM_ (putStrLn . JSON.toString) $ a
		["get", keyidHex, mname] -> do
			let keyid = hexToHash keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			metaM <- getMeta keyid mname
			case metaM of
				Nothing -> error "Meta not found"
				Just meta -> do
					contentM <- findMetaContent' meta
					case contentM of
						Nothing -> error "Meta content not found"
						Just content -> putStr content
		["get", keyidHex] -> do
			let keyid = hexToHash keyidHex
			vs <- getMetaNames keyid
			mapM_ (putStrLn) vs
		["edit", keyidHex, mname] -> do
			let keyid = hexToHash keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			v <- getMeta keyid mname
			case v of
				Nothing -> error "Meta not found"
				Just meta -> do
					cont <- findMetaContent' meta
					case cont of
						Nothing -> error "Meta content not found"
						Just cs -> do
							(fP, hdl) <- openTempFile "/tmp" "hellnetmeta"
							hPutStr hdl cs
							hClose hdl
							returnCode <- rawSystem "editor" [fP]
							case returnCode of
								ExitFailure i -> error $ "editor failed with code: " ++ show i
								ExitSuccess -> do
									modified <- readFile fP
									case JSON.fromString modified of
										Left errmsg -> error $ "JSON parsing error: " ++ errmsg
										Right _ -> do
											uri <- insertData theKey (BUL.fromString modified)
											newmetaM <- regenMeta $ meta {contentURI = uri}
											case newmetaM of
												Nothing -> error "Failed to re-sign meta"
												Just newmeta -> storeMeta newmeta
		["replace", keyidHex, mname] -> do
			let keyid = hexToHash keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			contentV <- getContents
			case JSON.fromString contentV of
				Left errmsg -> error $ "JSON parsing error: " ++ errmsg
				Right _ -> do
					contentURIV <- insertData theKey (BUL.fromString contentV)
					newMetaM <- regenMeta Meta {
						contentURI = contentURIV,
						keyID = keyid,
						timestamp = 0,
						message = Nothing,
						signature = Nothing,
						metaName = mname
						}
					case newMetaM of
						Nothing -> error "Failed to sign meta"
						Just newMeta -> storeMeta newMeta
		["genkey"] -> do
			putStrLn "Generating keys..."
			keyID <- generateKeyPair
			putStrLn $ "Your key ID is " ++ hashToHex keyID
		["new", keyidHex, mname] -> do
			let keyid = hexToHash keyidHex
			emptyUri <- insertData theKey $ BUL.fromString "{}"
			newMetaM <- regenMeta Meta {
				contentURI = emptyUri,
				keyID = keyid,
				timestamp = 0,
				message = Nothing,
				signature = Nothing,
				metaName = mname
				}
			case newMetaM of
				Nothing -> error "Failed to sign meta"
				Just newMeta -> storeMeta newMeta
		["alias", "add", name, keyidHex] -> do
			let keyid = hexToHash keyidHex
			storeKeyAliases $ Map.insert name keyid keyAliases
		["alias", "rm", name] -> do
			storeKeyAliases $ Map.delete name keyAliases
		["alias", "show", name] -> do
			putStrLn $ maybe (error "Alias not found") (hashToHex) $ Map.lookup name keyAliases
		["alias", "list"] -> do
			mapM_ (\(k, v) -> putStrLn $ k ++ ": " ++ hashToHex v) $ Map.toList keyAliases
		otherwise ->
			let usageStrings =  [ "",
				"update <key id> [<meta name>]    -- Update selected meta or all metas signed by key",
				"get <key id>                     -- output all meta names signed by key, one per line",
				"get <key id> <meta name>         -- display contents of meta",
				"get <key id> <meta name> <jpath> -- Display results of running jPath expression on meta content, one result per line",
				"edit <key id> <meta name>        -- launches `editor` to edit specified meta",
				"replace <key id> <meta name>     -- replaces meta contents with data from STDIN.",
				"genkey                           -- generates new key pair, displays key ID",
				"new <key id> <meta name>         -- creates new empty meta",
				"alias add <name> <key id>        -- adds new key alias",
				"alias rm <name>                  -- removes key alias",
				"alias show <name>                -- resolves alias to key id",
				"alias list                       -- shows all the aliases with their names"
				] in error $ usageInfo (intercalate "\n" $ map ("hell-meta "++) usageStrings) options ++ concat errs