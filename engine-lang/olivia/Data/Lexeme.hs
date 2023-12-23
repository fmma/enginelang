module Data.Lexeme where

import Data.Char ( isSpace, isAlphaNum )
import Text.ParserCombinators.ReadP
import qualified Text.Read.Lex as Lex

import Class.Parse

data Lexeme
    = Ident String
    | Number Double
    | String String
    | Comment String
    | Symbol String
    | Space
    deriving Show

parseSpace = do
    satisfy isSpace
    skipSpaces

parseIdent = do
    Lex.Ident x <- Lex.lex
    return $ x

parseDouble = do
    Lex.Number x <- Lex.lex
    return $ fromRational $ Lex.numberToRational x

parseString = do
    Lex.String x <- Lex.lex
    return $ x

parseComment = do
    satisfy (=='/')
    satisfy (=='/')
    x <- many (satisfy (/='\n'))
    satisfy (=='\n')
    return x

parseSymbol = do
    x <- satisfy (\x -> not (isSpace x || isAlphaNum x))
    return $ [x]

instance Parse Lexeme where
    parse =
        const Space <$> parseSpace <+
        Number <$> parseDouble <+
        String <$> parseString <+
        Comment <$> parseComment <+
        Ident <$> parseIdent <+
        Symbol <$> parseSymbol

parse' :: Parse a => ReadP a
parse' = skipSpaces >> ((parseComment >> skipSpaces >> parse) <+ parse)
