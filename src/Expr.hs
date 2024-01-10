{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Control.Applicative
import Control.Monad (join)
import Data.Attoparsec.Text

data Expr = Val Int | Div Expr Expr

parseExpr :: Parser Expr
parseExpr = parseVal <|> parseDiv
  where
    parseVal :: Parser Expr
    parseVal = Val <$> decimal
    parseDiv :: Parser Expr
    parseDiv = Div <$> parseSubExpr <* string " / " <*> parseSubExpr
    parseSubExpr :: Parser Expr
    parseSubExpr =
        char '('
            *> (parseDiv <|> parseVal)
            <* char ')'
            <|> parseVal

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = join $ safeDiv <$> eval x <*> eval y

parseAndEval :: Parser (Maybe Int)
parseAndEval = eval <$> parseExpr
