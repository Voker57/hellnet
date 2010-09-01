module Hellnet.FileTree (FileTree(..)) where

import qualified Data.Map                  as Map
import           Text.HJson                as JSON
import           Text.JSON.JPath

data FileTree = Dir Integer (Map.Map FilePath FileTree) | File Integer String deriving (Eq, Show)

instance Jsonable FileTree where
	fromJson j = case jPath "type" j of
		[JString "file"] -> case (jPath "modified" j, jPath "link" j) of
			([JNumber modTimestamp], [JString link]) -> Just $ File (round modTimestamp) link
			otherwise -> Nothing
		[JString "dir"] -> case (jPath "modified" j, jPath "contents" j) of
			([JNumber modTimestamp], [conts@(JObject _)]) -> case fromJson conts of
				Nothing -> Nothing
				Just mp -> Just $ Dir (round modTimestamp) mp
			otherwise -> Nothing
	toJson (File md link) = JObject $ Map.fromList [
		("type", toJson "file"),
		("modified", toJson md),
		("link", toJson link)
		]
	toJson (Dir md conts) = JObject $ Map.fromList [
		("type", toJson "dir"),
		("modified", toJson md),
		("contents", toJson conts)
		]