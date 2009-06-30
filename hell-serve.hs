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
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import Hellnet.Storage
import Hellnet.Network
import Network.HTTP.Lucu as Lucu
import Network
import System.Environment

handShakeOut = do
	fD <- inputForm(1000)
	host <- getRemoteAddr'
	let fMap = Map.fromList fD
	if "port" `Map.member` fMap && (not $ host `elem` ["localhost","127.0.0.1"]) then do
		let node = (host, read $ Map.findWithDefault "0" "port" fMap)
		res <- liftIO $ queryNodeGet "hello" node
		if res /= Nothing then do
			r <- liftIO $ addNode node
			if r then output "OK"
				else output "EXISTS"
			else output "FAIL"
		else
		setStatus NotAcceptable

main = do
	args <- getArgs
	let port = if length args == 0 then
		6666
		else
		fromIntegral (read (head args) :: Int) :: PortNumber
	let config = defaultConfig { cnfServerPort = PortNumber port };
	chunksPath <- toFullPath "store"
	metaPath <- toFullPath "meta"
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
	let resources = mkResTree [ (["chunks"], staticDir chunksPath), (["meta"], staticDir metaPath), (["hello"], helloRes), (["handshake"], handShakeRes) ]
	storeFile "serverport" (BS8.pack $ show port)
	runHttpd config resources []