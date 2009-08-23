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

module Hellnet.Utils (hashToHex, hexToHash, splitFor, stringToOctets, filt, filtM, unjust, splitBsFor, shuffle, genHash, discard, genKey, simpleOpts, encryptAES, decryptAES, splitBslFor, forkChild, splitInTwo, processOptions, mkUrl, explode, getUnixTime, safeRead)  where

import Codec.Encryption.AES
import Codec.Text.Raw
import Codec.Utils
import Control.Concurrent
import qualified Control.Exception as Ex
import Data.Foldable (foldlM)
import Data.LargeWord
import Data.List
import Data.Time
import Data.Word
import Hellnet
import Numeric
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8 (unpack,pack)
import qualified Data.ByteString.Lazy as BSL
import Random
import System.Locale
import Text.PrettyPrint.HughesPJ (render)

unjust :: (Maybe a) -> a
unjust (Just a) = a

hashToHex :: [Octet] -> String
hashToHex x = render (hexdumpBy "" 666 x)

hexToHash :: String -> [Octet]
hexToHash s = map (fst . head . readHex) (splitFor 2 s)

splitFor :: Int -> [a] -> [[a]]
splitFor _ [] = []
splitFor n xs = (take n xs) : (splitFor n (drop n xs))

-- splitBsFor :: Int -> BS -> [BS]
splitBsFor n xs = if BS.null xs then
	[]
	else
	(BS.take n xs) : (splitBsFor n (BS.drop n xs))

splitBslFor :: Int -> BSL.ByteString -> [BSL.ByteString]
splitBslFor n xs = if BSL.null xs then
	[]
	else
	(BSL.take (fromIntegral n) xs) : (splitBslFor n (BSL.drop (fromIntegral n) xs))

stringToOctets :: String -> [Octet]
stringToOctets s = BS.unpack $ BS8.pack s

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
	return (map (fromIntegral) (take hashSize (randomRs (0,255) gen) :: [Int]))

-- | discards return value and to make matters worse, taint the result
discard :: a -> IO ()
discard _ = return ()

genKey :: IO Key
genKey = do
	gen <- newStdGen
	return (map (fromIntegral) (take encKeySize (randomRs (0,255) gen) :: [Int]))

-- | parses command line options, splits flags and args
simpleOpts :: [String] -> ([String], [String])
simpleOpts ss = partition ((== '-') . head) ss

decryptAES :: [Octet] -> [Octet] -> [Octet]
decryptAES key os = listToOctets $ map (decrypt ((fromOctets 256 key) :: Word256)) $ listFromOctets os

encryptAES :: [Octet] -> [Octet] -> [Octet]
encryptAES key os = listToOctets $ map (encrypt ((fromOctets 256 key) :: Word256)) $ listFromOctets os

forkChild :: IO a -> IO (MVar a)
forkChild m = do
	v <- newEmptyMVar
	forkIO $ putMVar v =<< m
	return v

splitInTwo :: Char -> String -> (String, String)
splitInTwo c s = let broken = break (== c) s in ((fst broken), (tail $ snd broken))

processOptions def = foldl (flip ($)) def

mkUrl :: Node -> String -> String
mkUrl (h,p) s = "http://" ++ (h) ++ ":" ++ (show p) ++ "/" ++ s

explode :: Char -> String -> [String]
explode c = unfoldr (\s -> if null s then Nothing else Just (takeWhile (/=c) s, (safeTail . dropWhile (/=c)) s)) where safeTail l = if null l then [] else tail l

getUnixTime :: IO Integer
getUnixTime = do
	tim <- getCurrentTime
	return $ read $ formatTime defaultTimeLocale "%s" tim

safeRead :: (Read a) => String -> IO (Maybe a)
safeRead s = Ex.catch (Ex.evaluate (read s)) (\(Ex.ErrorCall _) -> return Nothing)