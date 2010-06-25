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

module Hellnet.Utils (
	breakLazySubstring
	, discard
	, explode
	, filt
	, filtM
	, forkChild
	, genHash
	, genKey
	, getUnixTime
	, hashToHex
	, hexToHash
	, mkUrl
	, processOptions
	, relaxByteString
	, safeGetEnv
	, shuffle
	, simpleOpts
	, splitBsFor
	, splitBslFor
	, splitFor
	, splitInTwo
	, stringToOctets
	, unjust
	)  where

import Codec.Text.Raw
import Codec.Utils
import Control.Concurrent
import qualified Control.Exception as Ex
import Data.Foldable (foldlM)
import Data.LargeWord
import Data.List
import Data.Time.Clock.POSIX
import Data.Word
import Hellnet
import Network.URI
import Numeric
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import qualified Data.ByteString.Lazy as BSL
import Random
import Safe
import System.Environment
import System.IO.Error
import Text.PrettyPrint.HughesPJ (render)

unjust :: (Maybe a) -> a
unjust (Just a) = a

hashToHex :: [Octet] -> String
hashToHex x = render (hexdumpBy "" 666 x)

hexToHash :: String -> [Octet]
hexToHash s = map (fst . head . readHex) $ splitFor 2 $ if odd (length s) then '0':s else s

splitFor :: Integer -> [a] -> [[a]]
splitFor _ [] = []
splitFor n xs = (genericTake n xs) : (splitFor n (genericDrop n xs))

-- splitBsFor :: Int -> BS -> [BS]
splitBsFor n xs = if BS.null xs then
	[]
	else
	(BS.take (fromIntegral n) xs) : (splitBsFor n (BS.drop (fromIntegral n) xs))

splitBslFor :: Integer -> BSL.ByteString -> [BSL.ByteString]
splitBslFor n xs = if BSL.null xs then
	[]
	else
	(BSL.take (fromIntegral n) xs) : (splitBslFor n (BSL.drop (fromIntegral n) xs))

stringToOctets :: String -> [Octet]
stringToOctets s = BS.unpack $ BU.fromString s

-- | filters list of values through list of filters
filt fs xs = foldl (flip ($)) xs fs

-- | filt for monads
filtM fs xs = foldlM (flip ($)) xs fs

-- | shuffles list
shuffle :: [a] -> IO [a]
shuffle xs = do
	gen <- newStdGen
	let zipd = zip xs (take (length xs) ((randoms gen) :: [Float]))
	return (map (fst) (sortBy (\a b -> (snd a) `compare` (snd b)) zipd))

genHash :: IO Hash
genHash = do
	gen <- newStdGen
	return (map (fromIntegral) (genericTake hashSize (randomRs (0,255) gen) :: [Int]))

-- | discards return value and to make matters worse, taint the result
discard :: a -> IO ()
discard _ = return ()

-- | generates random encryption key
genKey :: IO Key
genKey = do
	gen <- newStdGen
	return (map (fromIntegral) (genericTake encKeySize (randomRs (0,255) gen) :: [Int]))

-- | parses command line options, splits flags and args
simpleOpts :: [String] -> ([String], [String])
simpleOpts ss = partition ((== '-') . head) ss

forkChild :: IO a -> IO (MVar a)
forkChild m = do
	v <- newEmptyMVar
	forkIO $ putMVar v =<< m
	return v

-- | Splits string in two by certain char
splitInTwo :: Char -> String -> (String, String)
splitInTwo c s = let broken = break (== c) s in ((fst broken), (tail $ snd broken))

-- | handy func for optParse
processOptions def = foldl (flip ($)) def

-- | Makes URI for node
mkUrl :: Node -> String -> Network.URI.URI
mkUrl (h,p) s = let (pa, qe) = break (== '?') s in URI { -- FIXME: blah!
	uriScheme = "http:"
	, uriAuthority = Just URIAuth {
		uriUserInfo = ""
		, uriRegName = h
		, uriPort = ':' : show p
		}
	, uriPath = pa
	, uriQuery = qe
	, uriFragment = ""
	}

-- | Good old explode()
explode :: Char -> String -> [String]
explode c = unfoldr (\s -> if null s then Nothing else Just (takeWhile (/=c) s, (tailSafe . dropWhile (/=c)) s))

-- | Get current UNIX time
getUnixTime :: IO Integer
getUnixTime = do
	tim <- getPOSIXTime
	return $ round tim

relaxByteString :: BS.ByteString -> BSL.ByteString
relaxByteString = BSL.pack . BS.unpack

breakLazySubstring :: BSL.ByteString -- ^ String to search for
               -> BSL.ByteString -- ^ String to search in
               -> (BSL.ByteString,BSL.ByteString) -- ^ Head and tail of string broken at substring
breakLazySubstring pat src = search 0 src
	where
		search n s
			| BSL.null s = (src,BSL.empty) -- not found
			| pat `BSL.isPrefixOf` s = (BSL.take n src,s)
			| otherwise = search (n+1) (BSL.tail s)

safeGetEnv :: String -> IO (Maybe String)
safeGetEnv name = catch (getEnv name >>= (return . Just)) (\e -> if isDoesNotExistError e then return Nothing else ioError e)