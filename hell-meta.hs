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
import Data.Maybe
import Hellnet
import Hellnet.Crypto
import Hellnet.Meta as Meta
import Hellnet.Network
import Hellnet.Storage
import Hellnet.URI
import Hellnet.Utils
import Safe
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Cmd
import Text.HJson as JSON
import Text.Printf

data Opts = Opts {
	encrypt :: Bool,
	encKey :: Maybe Key,
	updateMeta :: Bool,
	encryptMeta :: Bool,
	metaEncKey :: Maybe Key
	}

options :: [OptDescr (Opts -> Opts)]
options = [
	Option ['m'] ["meta-encryption"]
		(OptArg (\s o ->  o {encryptMeta = True, metaEncKey = maybe (Nothing) (Just . hexToHash) s}) "key") "Use encryption on meta (optionally (if encrypting) with specified key)",
	Option ['e'] ["encrypt"]
		(OptArg (\s o ->  o {encrypt = True, encKey = maybe (Nothing) (Just . hexToHash) s}) "key") "Encrypt content (optionally with specified key)",
	Option ['u'] ["update-meta"]
		(NoArg (\o -> o {updateMeta = True})) "Automatically update meta before retrieval"
	]

defaultOptions = Opts {
	encrypt = False,
	encKey = Nothing,
	updateMeta = False,
	encryptMeta = False,
	metaEncKey = Nothing
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
	let preOpts = processOptions defaultOptions optz
	let (args', opts) = case (atMay args 1, parseHellnetURI (args !! 1)) of
		(Just _, Just (MetaURI keyid mnameM mpath encKey _)) -> do
			let
				namepath =	case (mnameM, mpath) of
					(Just mname, []) -> [mname]
					(Just mname, mp) -> [mname, mp]
					otherwise -> [];
				opts' = case encKey of
					Just k -> preOpts {encryptMeta = True, metaEncKey = Just k}
					otherwise -> preOpts
					in (head args : hashToHex keyid : namepath, opts')
		otherwise -> (args, preOpts)
	theMetaKey <- if encryptMeta opts then
		(return . Just) =<< (maybe (genKey) (return) $ metaEncKey opts)
		else
		return Nothing
	theKey <- if encrypt opts then
		(return . Just) =<< (maybe (genKey) (return) $ encKey opts)
		else
		return Nothing
	let ensureSuppliedMetaKey = when (isNothing (metaEncKey opts) && encryptMeta opts) (fail "You can't decrypt with random key!")
	let announceMetaKey = when (isNothing (metaEncKey opts) && encryptMeta opts) $ printf "Your meta key will be %s" (hashToHex $ fromMaybe (error "Meta key is going to be used but wasn't generated") theMetaKey)
	case args' of
		["update", keyidHex, mname] -> do
			keyid <- resolveKeyName keyidHex
			fetchMetaPrintResult keyid mname
		["update", keyidHex] -> do
			keyid <- resolveKeyName keyidHex
			allmeta <- getMetaNames keyid
			mapM_ (fetchMetaPrintResult keyid) allmeta
		["get", keyidHex, mname, mpath] -> do
			ensureSuppliedMetaKey
			keyid <- resolveKeyName keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			vs <- findMetaValue theMetaKey keyid mname mpath
			case vs of
				Nothing -> error "Meta not found"
				Just a -> mapM_ (putStrLn . JSON.toString) $ a
		["get", keyidHex, mname] -> do
			ensureSuppliedMetaKey
			keyid <- resolveKeyName keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			metaM <- getMeta keyid mname
			case metaM of
				Nothing -> error "Meta not found"
				Just meta -> do
					contentM <- findMetaContent' theMetaKey meta
					case contentM of
						Nothing -> error "Meta content not found"
						Just content -> BSL.putStr content
		["get", keyidHex] -> do
			keyid <- resolveKeyName keyidHex
			vs <- getMetaNames keyid
			mapM_ (putStrLn) vs
		["edit", keyidHex, mname] -> do
			keyid <- resolveKeyName keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			v <- getMeta keyid mname
			case v of
				Nothing -> error "Meta not found"
				Just meta -> do
					cont <- findMetaContent' theMetaKey meta
					case cont of
						Nothing -> error "Meta content not found"
						Just cs -> do
							(fP, hdl) <- openTempFile "/tmp" "hellnetmeta"
							BSL.hPut hdl cs
							hClose hdl
							returnCode <- rawSystem "editor" [fP]
							case returnCode of
								ExitFailure i -> error $ "editor failed with code: " ++ show i
								ExitSuccess -> do
									modified <- BSL.readFile fP
									uri <- insertData theKey (maybe (id) (encryptSym) theMetaKey $ modified)
									newmetaM <- regenMeta $ meta {contentURI = uri}
									case newmetaM of
										Nothing -> error "Failed to re-sign meta"
										Just newmeta -> do
											storeMeta newmeta
											ensureSuppliedMetaKey
		["replace", keyidHex, mname] -> do
			keyid <- resolveKeyName keyidHex
			when (updateMeta opts) (fetchMeta keyid mname >> return ())
			contentV <- BSL.getContents
			contentURIV <- insertData theKey (maybe (id) (encryptSym) theMetaKey $ contentV)
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
				Just newMeta -> do
					storeMeta newMeta
					announceMetaKey
		["genkey"] -> do
			putStrLn "Generating keys..."
			keyID <- generateKeyPair
			putStrLn $ "Your key ID is " ++ hashToHex keyID
		["new", keyidHex, mname] -> do
			keyid <- resolveKeyName keyidHex
			emptyUri <- insertData theKey $ maybe (id) (encryptSym) theMetaKey $ BUL.fromString "{}"
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
				Just newMeta -> do
					storeMeta newMeta
					announceMetaKey
		["rm", keyidHex, mname] -> do
			keyid <- resolveKeyName keyidHex
			res <- deleteMeta keyid mname
			when (not res) (fail "No such meta.")
		["alias", "add", name, keyidHex] -> do
			keyid <- resolveKeyName keyidHex
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
				"rm <key id> <meta name>          -- removes meta",
				"genkey                           -- generates new key pair, displays key ID",
				"new <key id> <meta name>         -- creates new empty meta",
				"alias add <name> <key id>        -- adds new key alias",
				"alias rm <name>                  -- removes key alias",
				"alias show <name>                -- resolves alias to key id",
				"alias list                       -- shows all the aliases with their names"
				] in error $ usageInfo (intercalate "\n" $ map ("hell-meta "++) usageStrings) options ++ concat errs