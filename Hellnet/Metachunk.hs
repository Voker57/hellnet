module Hellnet.Metachunk (Metachunk(..), fromString, fromLines) where

import Debug.Trace
import Hellnet.Utils

data Metachunk = Metachunk [(String, String)]
	deriving (Eq, Show)

fromString s =	fromLines $ lines s

toPairs (Metachunk ss) = ss

fromLines [] = Metachunk []
fromLines ss =
	let (ent, rest) = splitOn "" ss;
		therest = if null rest then [] else tail rest in
		Metachunk (if null ent then
			[]
			else
			(head ent, (init $ unlines $ tail ent)) : (toPairs (fromLines therest)))