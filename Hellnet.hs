module Hellnet (hashSize, chunkSize, hashesPerChunk) where

hashSize :: Int
hashSize = 64
chunkSize :: Int
chunkSize = 64 * 1024
hashesPerChunk :: Int
hashesPerChunk = 1023