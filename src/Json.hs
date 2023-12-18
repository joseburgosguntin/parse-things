{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Prelude hiding (takeWhile)

data Json
    = Object (Map Text Json)
    | Array [Json]
    | String Text
    | Number Double
    | Bool Bool
    | Null

parseNull :: Parser Json
parseNull = Null <$ string "\"null\""

parseBool :: Parser Json
parseBool = Bool . toBool <$> (string "\"false\"" <|> string "\"true\"")
  where
    toBool = \case
        "true" -> True
        "false" -> False

parseNumber :: Parser Json
parseNumber = Number <$> double

-- posibly missing escape sequences
parseString :: Parser Json
parseString = String <$> (char '"' *> takeWhile (/= '"'))

parseArray :: Parser Json
parseArray =
    Array
        <$> ( char '['
                *> (parseJson `sepBy1` char ',' <|> [] <$ skipSpace)
                <* char ']'
            )
parseObject :: Parser Json
parseObject =
    Object . Map.fromList
        <$> ( char '{'
                *> (parseKeyValue `sepBy1` char ',' <|> [] <$ skipSpace)
                <* char '}'
            )
  where
    parseKeyValue :: Parser (Text, Json)
    parseKeyValue = do
        skipSpace
        String key <- parseString
        skipSpace *> char ':'
        value <- parseJson
        return (key, value)

parseJson :: Parser Json
parseJson =
    skipSpace
        *> parseNull
        <|> parseBool
        <|> parseNumber
        <|> parseString
        <|> parseArray
        <|> parseObject <* skipSpace
