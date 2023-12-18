{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Control.Applicative
import Data.Attoparsec.Text

data Expr = Val Int | Div Expr Expr

parseExpr :: Parser Expr
parseExpr = parseExpr <|> parseDiv
  where
    parseVal :: Parser Expr
    parseVal = parseOptionalParens $ Val <$> decimal
    parseDiv :: Parser Expr
    parseDiv = parseOptionalParens $ Div <$> parseExpr <* string " / " <*> parseExpr
    parseOptionalParens :: Parser a -> Parser a
    parseOptionalParens p = char '(' *> p <* char ')' <|> p

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y

eval :: Expr -> Maybe Int
eval (Val n) = return n
eval (Div x y) = do
    n <- eval x
    m <- eval y
    safeDiv n m

parseAndEval :: Parser (Maybe Int)
parseAndEval = eval <$> parseExpr
