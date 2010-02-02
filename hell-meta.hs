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

import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.ByteString.Lazy as BSL
import Hellnet
import Hellnet.Meta as Meta
import Hellnet.Network
import Hellnet.Storage
import Hellnet.Utils
import System.Environment
import System.Exit
import System.IO
import System.Cmd
import Text.HJson as JSON

fetchMetaPrintResult :: KeyID -> String -> IO ()
fetchMetaPrintResult keyid mname = do
	result <- fetchMeta keyid mname
	let metaName = hashToHex (take 10 keyid) ++ ".../" ++ mname
	putStrLn $ case result of
				True -> "Meta "++ metaName ++" updated"
				False -> "Meta "++ metaName ++" unchanged"

main = do
	args <- getArgs
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
			vs <- findMetaValue keyid mname mpath
			case vs of
				Nothing -> error "Meta not found"
				Just a -> mapM_ (putStrLn . JSON.toString) $ a
		["get", keyidHex, mname] -> do
			let keyid = hexToHash keyidHex
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
											uri <- insertData Nothing (BUL.fromString modified)
											newmetaM <- regenMeta $ meta {contentURI = uri}
											case newmetaM of
												Nothing -> error "Failed to re-sign meta"
												Just newmeta -> storeMeta newmeta
		["genkey"] -> do
			putStrLn "Generating keys..."
			keyID <- generateKeyPair
			putStrLn $ "Your key ID is " ++ hashToHex keyID
		["new", keyidHex, mname] -> do
			let keyid = hexToHash keyidHex
			emptyUri <- insertData Nothing $ BUL.fromString "{}"
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