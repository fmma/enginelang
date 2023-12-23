module Parser (
    Parser(..), 
    (<|>),
    empty, 
    satisfy, 
    many, 
    many1, 
    sepBy,
    sepBy1,
    chainl,
    chainr,
    tok,
    toks,
    wtok,
    wtoks,
    parseWhitespace,
    parseWhitespace1,
    parseInt,
    parseFloat,
    parseString,
    parseRead,
    guard
    ) 
where

import Control.Applicative
import Data.Char

newtype Parser e = Parser { runParser :: String -> [(e, String)] } deriving Functor

instance Applicative Parser where
    pure x = Parser (\w -> [(x, w)])
    Parser p1 <*> Parser p2 =
        Parser (\ w -> do
            (f, w1) <- p1 w
            (x, w2) <- p2 w1
            return (f x, w2)
        )

instance Monad Parser where
    return = pure
    Parser p1 >>= f =
        Parser (\w -> do 
            (x, w1) <- p1 w
            runParser (f x) w1
            )

instance Alternative Parser where
    Parser p1 <|> Parser p2 = Parser (\w -> take 1 $ p1 w ++ p2 w)
    empty = Parser (const [])

guard :: (String -> Bool) -> Parser ()
guard p =
    Parser $ \w -> if p w then [((), w)] else []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser (\w -> 
    case w of
        c:w0 | p c -> [(c, w0)]
        _ -> [])

many1 :: Parser e -> Parser [e]
many1 p = (:) <$> p <*> many p

sepBy1 :: Parser a -> Parser e -> Parser [e]
sepBy1 op e = (:) <$> e <*> many (op *> e)

sepBy :: Parser a -> Parser e -> Parser [e]
sepBy op e = sepBy1 op e <|> pure []

chainl :: Parser (a -> a -> a) -> Parser a -> Parser a
chainl op e = do 
    x <- e
    xs <- many (flip <$> op <*> e)
    return $ foldl (flip ($)) x xs

chainr :: Parser (a -> a -> a) -> Parser a -> Parser a
chainr op e = (do
    x <- e
    f <- op
    y <- chainr op e
    return $ f x y) <|> e

tok :: String -> Parser String
tok t = go t
    where
        go [] = pure ""
        go (c:cs) = (:) <$> satisfy (==c) <*> go cs

toks :: [String] -> Parser String
toks ts = foldl1 (<|>) (map tok ts)

wtok :: String -> Parser String
wtok t = parseWhitespace *> tok t

wtoks :: [String] -> Parser String
wtoks ts = foldl1 (<|>) (map wtok ts)

parseWhitespace :: Parser String
parseWhitespace = many (satisfy isSpace)

parseWhitespace1 :: Parser String
parseWhitespace1 = many1 (satisfy isSpace)

parseInt :: Parser Int
parseInt = parseRead

parseFloat :: Parser Float
parseFloat = parseRead

parseString :: Parser String
parseString = parseRead

parseRead :: Read a => Parser a
parseRead = Parser reads

-- end module parser