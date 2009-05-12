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

import Hellnet.Network
import System.Environment
import Hellnet.Storage
import Control.Exception (evaluate)

node ss = ( (ss !! 1), (read (ss !! 2)) :: Int )

main = do
	nodes <- nodesList
	evaluate nodes
	args <- getArgs
	if and [((length args) == 3), ((head args) == "add")] then
		writeNodesList ((node args) : nodes)
		else if and [((length args) == 3), ((head args) == "rm")] then
			writeNodesList (dropWhile (== (node args)) nodes)
			else if and [((length args) == 1), ((head args) == "clear")] then
				writeNodesList []
				else if and [((length args) == 1), ((head args) == "list")] then
					print nodes
					else
					putStrLn "Usage: hell-nodes {add,rm,clear,list} <host> <port>"