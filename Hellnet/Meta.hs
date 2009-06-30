module Hellnet.Meta where

import Hellnet.Utils

data Meta = Meta String String deriving Show -- key value

metaFromTuple :: (String, String) -> Meta
metaFromTuple (k, v) = Meta k v

metaFromString :: String -> Meta
metaFromString s = let (a, b) = splitInTwo ':' s in Meta a b