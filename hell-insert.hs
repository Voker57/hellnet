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

import Hellnet.Files
import Hellnet.Utils
import System.Environment (getArgs)
import Control.Monad

insertFilePrintHash fname = do
	hsh <- insertFile fname
	putStrLn (fname ++ ": hell://file/" ++ (hashToHex hsh))

main = do
	args <- getArgs
	when (length args == 0) (putStrLn "Usage: hell-insert <file1> [<file2>...]")
	mapM (insertFilePrintHash) args
	putStr ""