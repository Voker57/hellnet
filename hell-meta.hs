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

import Hellnet
import Hellnet.Network
import Hellnet.Storage
import Hellnet.Utils
import System.Environment

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