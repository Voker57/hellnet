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

{-# LANGUAGE BangPatterns #-}

import Control.Exception (evaluate)
import Hellnet.Network
import Hellnet.Storage
import System.Environment

node h p = ( h, read p :: Int )

main = do
	!nodes <- getNodesList
	args <- getArgs
	case args of
		["add", host, port] -> writeNodesList (node host port : nodes)
		["rm", host, port] -> writeNodesList (dropWhile (== node host port) nodes)
		["clear"] -> writeNodesList []
		["list"] -> print nodes
		["handshake", host, port] -> do
			result <- handshakeWithNode (node host port)
			putStrLn $ if result then "Handshake successful" else "Handshake failed"
		otherwise -> putStrLn "Usage: hell-nodes {add,rm,clear,list, discover} <host> <port>"
