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

import Hellnet.Storage (toFullPath)
import Network.HTTP.Lucu as Lucu
import Network
import System.Environment

main = do
	args <- getArgs
	let port = if length args == 0 then
		6666
		else
		fromIntegral (read (head args) :: Int) :: PortNumber
	let config = defaultConfig { cnfServerPort = PortNumber port };
	chunksPath <- toFullPath "store"
	metaPath <- toFullPath "meta"
	let helloRes = ResourceDef {
		resUsesNativeThread = False,
		resIsGreedy = True,
		resGet = Just $ output "Why, hello",
		resHead = Nothing,
		resPost = Nothing,
		resPut = Nothing,
		resDelete = Nothing
		}
	let resources = mkResTree [ (["chunks"], staticDir chunksPath), (["meta"], staticDir metaPath), (["hello"], helloRes) ]
	runHttpd config resources []