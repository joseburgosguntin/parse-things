{-# LANGUAGE OverloadedStrings #-}

module ParseInput (parseInput, printInput) where

import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text, pack)
import System.Directory (getCurrentDirectory)

parseInput :: String -> Parser a -> IO a
parseInput input parser = do
    dir <- getCurrentDirectory
    let inputPath = dir <> "/input/" <> input <> ".txt"
    txt <- fromFile inputPath
    case parseOnly parser txt of
        Right res -> return res
        Left e -> fail e
  where
    fromFile :: FilePath -> IO Text
    fromFile path = pack <$> readFile path

printInput :: (Show a) => String -> Parser a -> IO ()
printInput input parser = parseInput input parser >>= print
