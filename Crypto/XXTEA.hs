module Crypto.XXTEA (encrypt, decrypt) where

import Codec.Utils
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Data.List
import Data.STRef
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word

type XXTEAKey = Array Word8 Word32

-- | Encrypts message with given key. Message must be more than 4 bytes.
encrypt :: (Word32, Word32, Word32, Word32) -> [Octet] -> [Octet]
encrypt (k1, k2, k3, k4) os =
	let xkey = array (0, 3) [(0, k1), (1, k2), (2, k3), (3, k4)];
		w32s = listFromOctets os in
			listToOctets $ encrypt' xkey w32s

-- | Decrypts message with given key. Message must be more than 4 bytes.
decrypt :: (Word32, Word32, Word32, Word32) -> [Octet] -> [Octet]
decrypt (k1, k2, k3, k4) os =
 	let xkey = array (0, 3) [(0, k1), (1, k2), (2, k3), (3, k4)];
 		w32s = listFromOctets os in
 			listToOctets $ decrypt' xkey w32s 

encrypt' :: XXTEAKey -> [Word32] -> [Word32]
encrypt' k ls = runST $ do
	ia <- (newListArray (0, fromIntegral (length ls) - 1) ls) :: ST s (STArray s Integer Word32)
	(lbound, rbound) <- getBounds ia
	let nitems = genericLength [lbound..rbound]
	let nrounds = 6 + 52 `div` nitems
	z <- readArray ia (nitems-1)
	zV <- newSTRef z
	yV <- newSTRef 0 :: ST s (STRef s Word32)
	eV <- newSTRef 0 :: ST s (STRef s Word32)
	let mx z s y e p = do
		let kp = k ! (fromIntegral ((p .&. 3) `xor` e))
		return (((z `shiftR` 5 `xor` y `shiftL` 2) + (y `shiftR` 3 `xor` z `shiftL` 4)) `xor` ((s `xor` y) + (kp `xor` z)))
	let iterate n = do
		let summ = n * 0x9e3779b9
		writeSTRef eV (summ `shiftR` 2 .&. 3)
		mapM (\p -> do
			readArray ia (p + 1) >>= writeSTRef yV
			prevV <- readArray ia p
			z <- readSTRef zV
			y <- readSTRef yV
			e <- readSTRef eV
			newV <- mx z summ y e (fromIntegral p)
			writeArray ia p (prevV + newV)
			readArray ia p >>= writeSTRef zV
			) $ map (fromIntegral) [0..nitems-2]
		readArray ia 0 >>= writeSTRef yV
		prevV <- readArray ia (nitems - 1)
		z <- readSTRef zV
		y <- readSTRef yV
		e <- readSTRef eV
		newV <- mx z summ y e (fromIntegral nitems-1)
		writeArray ia (nitems - 1) (prevV + newV)
		readArray ia (fromIntegral nitems-1) >>= writeSTRef zV
	mapM (iterate . fromIntegral) [1..nrounds]
	getElems ia

decrypt' k ls = runST $ do
	ia <- (newListArray (0, fromIntegral (length ls) - 1) ls) :: ST s (STArray s Integer Word32)
	(lbound, rbound) <- getBounds ia
	let nitems = genericLength [lbound..rbound]
	let nrounds = (6 + 52 `div` nitems)
	y <- readArray ia 0
	zV <- newSTRef 0 :: ST s (STRef s Word32)
	yV <- newSTRef y
	eV <- newSTRef 0 :: ST s (STRef s Word32)
	let mx z s y e p = do
		let kp = k ! (fromIntegral ((p .&. 3) `xor` e))
		return (((z `shiftR` 5 `xor` y `shiftL` 2) + (y `shiftR` 3 `xor` z `shiftL` 4)) `xor` ((s `xor` y) + (kp `xor` z)))
	let iterate n = do
		let summ = n * 0x9e3779b9
		writeSTRef eV (summ `shiftR` 2 .&. 3)
		mapM (\p -> do
			readArray ia (p - 1) >>= writeSTRef zV
			prevV <- readArray ia p
			z <- readSTRef zV
			y <- readSTRef yV
			e <- readSTRef eV
			newV <- mx z summ y e (fromIntegral p)
			writeArray ia p (prevV - newV)
			readArray ia p >>= writeSTRef yV
			) $ map (fromIntegral) $ reverse [1..nitems-1]
		readArray ia (nitems - 1) >>= writeSTRef zV
		prevV <- readArray ia 0
		z <- readSTRef zV
		y <- readSTRef yV
		e <- readSTRef eV
		newV <- mx z summ y e 0
		writeArray ia 0 (prevV - newV)
		readArray ia 0 >>= writeSTRef yV
	mapM (iterate . fromIntegral) $ reverse [1..nrounds]
	getElems ia

--       n = -n;
--       rounds = 6 + 52/n;
--       sum = rounds*DELTA;
--       y = v[0];
--       do {
--         e = (sum >> 2) & 3;
--         for (p=n-1; p>0; p--) {
--           z = v[p-1];
--           y = v[p] -= MX;
--         }
--         z = v[n-1];
--         y = v[0] -= MX;
--       } while ((sum -= DELTA) != 0);

-- #define DELTA 0x9e3779b9
-- #define MX (((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (k[(p&3)^e] ^ z)))
-- void btea(uint32_t *v, int n, uint32_t const k[4]) {
--     uint32_t y, z, sum;
--     unsigned p, rounds, e;
--     if (n > 1) {          /* Coding Part */
--       rounds = 6 + 52/n;
--       sum = 0;
--       z = v[n-1];
--       do {
--         sum += DELTA;
--         e = (sum >> 2) & 3;
--         for (p=0; p<n-1; p++) {
--           y = v[p+1];
--           v[p] += MX;
--           z = v[p];
--         }
--         y = v[0];
--         v[n-1] += MX;
--         z = v[n-1];
--       } while (--rounds);
--     } else if (n < -1) {  /* Decoding Part */
--       n = -n;
--       rounds = 6 + 52/n;
--       sum = rounds*DELTA;
--       y = v[0];
--       do {
--         e = (sum >> 2) & 3;
--         for (p=n-1; p>0; p--) {
--           z = v[p-1];
--           y = v[p] -= MX;
--         }
--         z = v[n-1];
--         y = v[0] -= MX;
--       } while ((sum -= DELTA) != 0);
--     }
--   }