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

module Hellnet.ExternalChunks (ChunkLocation(..)) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Maybe
import Hellnet
import Hellnet.Crypto
import Hellnet.Utils
import System.Directory
import System.IO
import Text.HJson

data ChunkLocation = FileLocation FilePath Integer (Maybe Key) deriving (Ord, Eq, Show)

instance Jsonable ChunkLocation where
	toJson (FileLocation path offset encKey) = JObject $ Map.fromList $ [
		("type", toJson "file"),
		("path", toJson path),
		("offset", toJson offset)] ++ (maybeToList $ fmap ((\a -> ("key", a)) . toJson . crockford) encKey)
	fromJson (JObject m) = case map (flip Map.lookup $ m) ["type", "path", "offset", "key"] of
		[Just (JString "file"), Just (JString path), Just (JNumber offset), keyM] -> Just $ FileLocation path (round offset) (fmap (\(JString s) -> decrockford s) keyM)
		otherwise -> Nothing
	fromJson _ = Nothing
