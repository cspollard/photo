{-# LANGUAGE OverloadedStrings #-}

module Data.Tiff where

import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Attoparsec.ByteString.Lazy as ABSL

import Control.Applicative

data ByteOrder = II | MM deriving Show

byteOrder :: Parser ByteOrder
byteOrder = (string "II" *> return II) <|>
                (string "MM" *> return MM)

data IFD = IFD deriving Show

ifds :: Int -> BSL.ByteString -> [IFD]
ifds 0 = []
ifds addr = 

data Header = Header ByteOrder [IFD] deriving Show

header :: Parser Header
header = Header <$> byteOrder <*> ifds


data TiffValue = TiffByte
                | TiffAscii
                | TiffShort
                | TiffLong
                | TiffRational
