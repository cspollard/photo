{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BSC
import Data.Attoparsec.ByteString.Lazy hiding (take, map, drop)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import Control.Applicative
import Data.Maybe (catMaybes)

import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

import Data.Word


digits :: Int -> Parser Int
digits n = fmap (read . BSC.unpack) $ A.take n


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



main :: IO ()
main = do
    outfolder:infiles <- getArgs

    outfilenames' <- sequence $ map findTimestamp infiles :: IO [Maybe LocalTime]

    print outfilenames'
