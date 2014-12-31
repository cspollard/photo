{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import Data.Attoparsec.ByteString.Lazy (Parser, word8, maybeResult, parse, anyWord8)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy as BSL

import Control.Applicative
import Data.Maybe (fromJust, isJust)

import Data.Time (fromGregorian, LocalTime(..), TimeOfDay(..), formatTime, defaultTimeLocale)

import Data.Word
import System.Directory (renameFile, doesFileExist, createDirectoryIfMissing)


digits :: Int -> Parser Int
digits n = (read . BSC.unpack) <$> A.take n


char :: Char -> Parser Word8
char = word8 . fromIntegral . fromEnum


timestamp :: Parser LocalTime
timestamp = do
    y <- fmap fromIntegral $ digits 4 <* char ':'
    m <- digits 2 <* char ':'
    d <- digits 2 <* char ' '

    hr <- digits 2 <* char ':'
    mi <- digits 2 <* char ':'
    se <- fmap fromIntegral $ digits 2 <* char '\0'

    return $
        LocalTime (fromGregorian y m d) (TimeOfDay hr mi se)



findTimestamp :: String -> IO (Maybe LocalTime)
findTimestamp = fmap (maybeResult . parse ts) . BSL.readFile
    where ts = timestamp <|> (anyWord8 *> ts)


tryMove :: FilePath -> FilePath -> Int -> String -> IO ()
tryMove fin fout n suffix = do
    let outfile = fout ++ "." ++ show n ++ suffix
    exists <- doesFileExist outfile
    if exists
        then do
            print $ "file " ++ outfile ++ " already exists; incrementing suffix."
            tryMove fin fout (n+1) suffix
        else renameFile fin outfile


moveImage :: String -> String -> IO ()
moveImage fname dir = do
    t <- findTimestamp fname
    let suff = '.' : (reverse . takeWhile (/= '.') . reverse $ fname)

    case t of
        Nothing -> do
            print $ "error: " ++ fname ++ " does not have a time stamp; continuing."
            return ()

        Just ts -> do
            let outfolder = ((++) dir . formatTime defaultTimeLocale "/%0Y/%m") ts
            createDirectoryIfMissing True outfolder
            tryMove fname (outfolder ++ formatTime defaultTimeLocale "/%d-%H.%M.%S" ts) 0 suff



main :: IO ()
main = do
    outfolder:infiles <- getArgs

    mapM_ (flip moveImage outfolder) infiles
