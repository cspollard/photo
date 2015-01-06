module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BSL
import Data.Attoparsec.ByteString.Lazy (parse, maybeResult)
import Data.Tiff

main :: IO ()
main = do
    fname <- fmap head getArgs

    bs <- BSL.readFile fname

    let h = maybeResult $ parse header bs

    print h
