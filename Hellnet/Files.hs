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

module Hellnet.Files (insertFile, downloadFile, indexFile, indexData, downloadFileChunks, downloadFileToHandle, downloadFileChunksToHandle) where

import Codec.Utils
import Data.Foldable (foldrM)
import Data.Maybe
import Hellnet
import Hellnet.ExternalChunks
import Hellnet.Network
import Hellnet.Storage
import Hellnet.URI
import Hellnet.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import System.FilePath
import System.IO
import System.Posix

insertFile :: Maybe Key -> FilePath -> IO Hash
insertFile encKey fname = do
	conts <- BSL.readFile fname
	hsh <- insertFileContents encKey conts
	return hsh

indexFile :: Maybe Key -> FilePath -> IO Hash
indexFile encKey fname = do
	fullPath <- canonicalizePath fname
	fH <- openFile fname ReadMode
	hSetBinaryMode fH True
	let indexFileLoop hs offset = do
		chunk <- BSL.hGet fH (fromIntegral chunkSize)
		hsh <- indexChunk encKey chunk (FileLocation fullPath offset encKey)
		eof <- hIsEOF fH
		if eof then
			return $ hs ++ [hsh]
			else
			indexFileLoop (hs ++ [hsh]) (offset + chunkSize)
	chunkHashes <- indexFileLoop [] 0
	let fileLink = splitFor hashesPerChunk chunkHashes
	let fileLinkChunks = Prelude.map (BSL.pack . concat) fileLink
	fileLinkHead <- foldrM (hashAndAppend encKey) BSL.empty (fileLinkChunks)
	fileLinkHash <- insertChunk encKey fileLinkHead
	return fileLinkHash

indexData :: Maybe Key -> FilePath -> IO HellnetURI
indexData encKey fpath = do
	fstatus <- getFileStatus fpath
	if toInteger (fileSize fstatus) > chunkSize then do
		hsh <- indexFile encKey fpath
		return $ FileURI hsh encKey (Just $ snd $ splitFileName fpath)
		else do
		dat <- BSL.readFile fpath
		fpathAbs <- canonicalizePath fpath
		hsh <- indexChunk encKey dat (FileLocation fpathAbs 0 encKey)
		return $ ChunkURI hsh encKey (Just $ snd $ splitFileName fpath)

getChunkAppendToHandle :: Maybe Key -> Handle -> Hash -> IO ()
getChunkAppendToHandle encKey h hsh = do
	conts <- getChunk encKey hsh
	maybe (error ("chunk not found in storage: " ++ (crockford hsh))) (BSL.hPut h) conts

downloadFileToHandle :: Maybe Key -> Handle -> Hash -> IO ()
downloadFileToHandle encKey h hs = do
	hsE <- fetchFile encKey hs
	case hsE of
		Left _ -> return ()
		Right hs -> downloadFileChunksToHandle encKey h hs

downloadFileChunksToHandle :: Maybe Key -> Handle -> [Hash] -> IO ()
downloadFileChunksToHandle encKey h hs = do
	mapM_ (getChunkAppendToHandle encKey h) hs

downloadFile :: Maybe Key -> FilePath -> Hash -> IO ()
downloadFile encKey fpath hsh = do
	h <- openFile fpath WriteMode
	downloadFileToHandle encKey h hsh
	hClose h

downloadFileChunks :: Maybe Key -> FilePath -> [Hash] -> IO ()
downloadFileChunks encKey fpath hs = do
	h <- openFile fpath WriteMode
	downloadFileChunksToHandle encKey h hs
	hClose h