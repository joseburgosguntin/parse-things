{-# LANGUAGE OverloadedStrings #-}

module CLang where

import Control.Applicative
import qualified Data.Attoparsec.Internal.Types as Atto
import Data.Attoparsec.Text
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Text (Text, cons, unpack)
import Foreign.C (
    CChar,
    CDouble,
    CFloat,
    CInt,
    CLLong,
    CLong,
    CShort,
 )
import Prelude hiding (takeWhile)

data IntLiteral
    = CChar !CChar
    | CShort !CShort
    | CInt !CInt
    | CLong
    | CLLong !CLLong
    deriving (Show)
data FloatLiteral
    = CFloat !CFloat
    | CDouble !CDouble
    deriving (Show)
data Literal
    = IntValue !IntLiteral
    | FloatValue !FloatLiteral
    deriving (Show)

parseLiteral :: Parser Literal -- weird space after needed
parseLiteral =
    (IntValue . CInt <$> decimal)
        <|> (FloatValue . CDouble <$> rational)

parseName :: Parser Text
parseName = cons <$> first <*> rest
  where
    first = satisfy $ inClass "a-zA-Z_"
    rest = takeWhile (inClass "a-zA-Z_0-9")

newtype Ident = Ident Text deriving (Show)

parseIdent :: Parser Ident
parseIdent = Ident <$> parseName

data Expr
    = Literal Literal
    | Variable Ident
    | FunctionCall Ident [Expr] -- fix
    deriving (Show)

parseExpr :: Parser Expr
parseExpr =
    (Literal <$> parseLiteral)
        <|> ( FunctionCall
                <$> parseIdent
                <*> parseParens
                    ( (parseExpr `sepBy` parseCommaSpace)
                        <|> ([] <$ skipSpace)
                    )
            )
        <|> (Variable <$> parseIdent)

newtype Type = Type Text deriving (Show)

parseType :: Parser Type
parseType = Type <$> parseName

data Type2
    = TypeName Text
    | Pointer Type2
    | Array Type2 Int
    | Parens Type2
    deriving (Show)

type AmountOfPointers = Int
type Capasity = Maybe Int

data TypeMod
    = TypeBasic AmountOfPointers Ident [Capasity]
    | TypeParens AmountOfPointers TypeMod [Capasity]
    deriving (Show)

parseTypeMod :: Parser TypeMod
parseTypeMod =
    TypeParens
        <$> parsePointers
        <* char '('
        <*> parseTypeMod
        <* char ')'
        <*> parseArrays
        <|> TypeBasic
            <$> parsePointers
            <*> parseIdent
            <*> parseArrays
  where
    parseArrays = many (char '[' *> optional decimal <* char ']')
    parsePointers = length <$> many (char '*')

data Type3
    = Type3 Text TypeMod
    deriving (Show)

parseType3 :: Parser Type3
parseType3 = Type3 <$> parseName <* skipSpace <*> parseTypeMod

data TypeMod2
    = TypeIdent Ident
    | TypePointer TypeMod2
    | TypeArray Int Capasity
    deriving (Show)

parseTypeMod2 :: Parser TypeMod2
parseTypeMod2 =
    parseTypeArray
  where
    -- <|> (TypeIdent <$> parseIdent)

    -- <|> (TypePointer <$> (char '*' *> parseTypeMod2))

    parseTypeArray = do
        -- h <- decimal <*> takeWhile1 (/= '[')
        -- h <- >=> (parse decimal)
        t <- takeWhile1 (/= '[') >>= return decimal
        -- let x = parse parseTypeMod2 "hey"
        TypeArray t <$> (char '[' *> optional decimal <* char ']')

-- a =  parseTypeMod2 . takeWhile1 (/= '[')
data Member
    = Member Type [Ident]
    deriving (Show)

parseMember :: Parser Member
parseMember =
    Member
        <$> parseType
        <* skipSpace
        <*> (parseIdent `sepBy` parseCommaSpace)

data MaybeInits
    = MaybeInits Type [(Ident, Maybe Expr)]
    deriving (Show)

parseInit :: Parser MaybeInits
parseInit =
    MaybeInits
        <$> parseType
        <* skipSpace
        <*> (parseInitial `sepBy` parseCommaSpace)
  where
    parseInitial :: Parser (Ident, Maybe Expr)
    parseInitial =
        (,)
            <$> parseIdent
            <* skipSpace
            <*> optional (skipSpace *> char '=' *> skipSpace *> parseExpr)

data Param
    = Param Type Ident
    deriving (Show)

parseParam :: Parser Param
parseParam = Param <$> parseType <* skipSpace <*> parseIdent

data Header
    = StdHeader Text
    | LocalHeader Text
    deriving (Show)

data Define
    = ConstDefine Ident Expr
    | ParamDefine Ident Statement
    deriving (Show)
data Directive
    = Include Header
    | DefineDirective Define
    | Undef Ident
    | IfDirective -- TODO
    | IfDef -- TODO
    | IfNDef -- TODO
    | Error Text
    | Pragma -- TODO
    deriving (Show)

-- parseDirectiveName :: Parser Text
-- parseDirectiveName = char '#' *> takeWhile (/= ' ')

parseInclude :: Parser Directive
parseInclude =
    Include <$> (string "#include" *> skipSpace *> (std <|> local))
  where
    std = char '<' *> (StdHeader <$> takeWhile (/= '>')) <* char '>'
    local = char '"' *> (LocalHeader <$> takeWhile (/= '"')) <* char '"'

parseDirective :: Parser Directive
parseDirective = parseInclude

data Statement
    = EmptyStatement
    | Unused Expr
    | Return Expr
    | VariableInits MaybeInits
    | Assignment Ident Expr
    | If Expr Body
    | Else Body
    | While Expr Body
    | For Expr Expr Expr Body
    | FunctionDeclaration Type Ident [Type]
    | FunctionDefinition Type Ident [Param] [Statement]
    | Struct Ident [Member]
    | Union Ident [Member]
    | PreprocesorDirective Directive -- not supper acurate
    deriving (Show)
type Body = [Statement]
type CFile = [Statement]

parseStatements :: Parser [Statement]
parseStatements =
    skipSpace
        *> ((parseStatement `sepBy` skipSpace) <|> ([] <$ skipSpace))
        <* skipSpace

parseBody :: Parser Body -- not done
parseBody = char '{' *> parseStatements <* char '}'

parseStatement :: Parser Statement
parseStatement =
    (EmptyStatement <$ char ';')
        <|> (PreprocesorDirective <$> parseDirective)
        <|> ( If
                <$> (string "if" *> skipSpace *> parseParens parseExpr)
                <* skipSpace
                <*> parseStatementOrBody
            )
        <|> ( Else
                <$> ( string "else"
                        *> skipSpace
                        *> parseStatementOrBody
                    )
            )
        <|> ( Return
                <$> ( string "return"
                        *> skipSpace
                        *> parseExpr
                        <* char ';'
                    )
            )
        <|> (VariableInits <$> parseInit <* char ';')
        <|> (Unused <$> parseExpr <* char ';')
        <|> ( Assignment
                <$> parseIdent
                <* skipSpace
                <* char '='
                <* skipSpace
                <*> parseExpr
                <* char ';'
            )
        <|> ( While
                <$> (string "while" *> parseParens parseExpr)
                <*> parseStatementOrBody
            )
        <|> ( FunctionDeclaration
                <$> parseType
                <* skipSpace
                <*> parseIdent
                <* skipSpace
                <*> parseParens
                    ( parseDeclarationParam
                        `sepBy` parseCommaSpace
                    )
                <* char ';'
            )
        <|> ( FunctionDefinition
                <$> parseType
                <* skipSpace
                <*> parseIdent
                <*> parseParens (parseParam `sepBy` parseCommaSpace)
                <* skipSpace
                <*> parseBody
            )
        <|> parseWithMembers Struct "struct"
        <|> parseWithMembers Union "union"
  where
    parseDeclarationParam =
        ((\(Param t _) -> t) <$> parseParam)
            <|> parseType

    parseStatementOrBody =
        parseBody
            <|> singleton <$> parseStatement
    parseWithMembers t s =
        t
            <$> ( string s
                    *> skipSpace
                    *> parseIdent
                )
            <* skipSpace
            <* char '{'
            <* skipSpace
            <*> many
                (parseMember <* parseSemiColonSpace)
            <* skipSpace
            <* char '}'
            <* char ';'

parseCommaSpace = char ',' *> skipSpace
parseSemiColonSpace = char ';' *> skipSpace
parseParens p = char '(' *> skipSpace *> p <* skipSpace <* char ')'
