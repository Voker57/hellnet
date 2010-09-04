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

import Control.Monad.Trans
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BUL
import Hellnet.ExternalChunks
import Hellnet.Storage
import Hellnet.Network
import Hellnet.Utils
import Network.HTTP.Lucu as Lucu
import Network
import System.FilePath
import System.Directory
import System.Environment

handShakeOut = do
	fD <- inputForm(1000)
	host <- getRemoteAddr'
	let fMap = Map.fromList fD
	if "port" `Map.member` fMap && (not $ host `elem` ["localhost","127.0.0.1"]) then do
		let node = (host, read $ BUL.toString $ fromMaybe (BUL.fromString "0") $ fmap (fdContent) $ Map.lookup "port" fMap)
		res <- liftIO $ queryNodeGet "hello" node
		if res /= Nothing then do
			r <- liftIO $ addNode node
			if r then output "OK"
				else output "EXISTS"
			else output "FAIL"
		else
		setStatus NotAcceptable

chunksHandler = do
	[fp, sp] <- getPathInfo
	let hsh = hexToHash (fp ++ sp)
	chunksPath <- liftIO $ toFullPath "store"
	exists <- liftIO $ doesFileExist $ joinPath [chunksPath, fp, sp]
	if exists then
		handleStaticFile $ joinPath [chunksPath, fp, sp]
		else do
		chunkIndex <- liftIO $ getIndex hsh
		case chunkIndex of
			Nothing -> setStatus NotFound
			Just loc -> do
				chunkM <- liftIO $ getExternalChunk loc
				case chunkM of
					Nothing -> setStatus NotFound
					Just chunk -> outputLBS chunk

main = do
	args <- getArgs
	let port = if length args == 0 then
		"6666"
		else
		head args
	let config = defaultConfig { cnfServerPort = port };
	metaPath <- toFullPath "meta"
	publicPath <- toFullPath "public.dsa"
	nodelistPath <- toFullPath "nodelist"
	let handShakeRes = ResourceDef {
		resUsesNativeThread = False,
		resIsGreedy = True,
		resGet = Nothing,
		resHead = Nothing,
		resPost = Just handShakeOut,
		resPut = Nothing,
		resDelete = Nothing
		}
	let helloRes = ResourceDef {
		resUsesNativeThread = False,
		resIsGreedy = True,
		resGet = Just $ output "Why, hello",
		resHead = Nothing,
		resPost = Nothing,
		resPut = Nothing,
		resDelete = Nothing
		}
	let chunksRes = emptyResource {
		resGet = Just $ chunksHandler,
		resIsGreedy = True
	}
	let resources = mkResTree [
		(["chunks"], chunksRes)
		,(["meta"], staticDir metaPath)
		,(["hello"], helloRes)
		,(["handshake"], handShakeRes)
		,(["nodelist"], staticFile nodelistPath)
		]
	storeFile "serverport" (BUL.fromString $ show port)
	putStrLn $ "Listening on port " ++ show port
	runHttpd config resources []