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

import Control.Monad
import qualified Data.Set as Set
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
			case result of
				Left errmsg -> do
					putStrLn $ "Handshake failed: " ++ errmsg
				Right True  -> do
					putStrLn "Handshake successful"
					addNode $ node host port
					return ()
				Right False -> do
					putStrLn "Handshake failed: port not open?"
		["discover"] -> do
			when (null nodes) (fail "Need at least one pre-discovered node! Use `hell-nodes handshake` to manually discover one")
			moreNodes <- mapM (fetchNodeListFromNode) nodes
			!nodes2 <- getNodesList -- Because we need it after discovery, to reduce desync
			let nset = Set.unions $ map (Set.fromList) (moreNodes)
			putStrLn $ show (length $ Set.toList $ Set.difference nset $ Set.fromList nodes2) ++ " new nodes discovered"
			writeNodesList $ Set.toList $ Set.union (Set.fromList nodes2) nset
			putStrLn "Handshaking..."
			results <- mapM (handshakeWithNode) $ Set.toList nset
			putStrLn $ (show $ length $ filter (== Right True) results) ++ " out of " ++ (show $ length results) ++ " handshakes successful."
		otherwise -> putStrLn "Usage: hell-nodes {add,rm,clear,list, discover} <host> <port>"
