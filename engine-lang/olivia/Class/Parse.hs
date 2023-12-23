module Class.Parse where

import Text.ParserCombinators.ReadP

class Parse a where
    parse :: ReadP a

-- Lower precedence then <$> and <*>
infixr 3 <+
infixr 3 <+>
(<+) = (<++)
(<+>) = (+++)

runParser :: Parse a => String -> [(a, String)]
runParser = readP_to_S parse

parseMaybe :: Parse a => String -> Maybe a
parseMaybe x = case parseList x of
    [x] -> Just x
    _ -> Nothing

parseList :: Parse a => String -> [a]
parseList x = map fst $ filter ((=="") . snd) (runParser x)

parseEither :: Parse a => String -> Either String a
parseEither x = case parseList x of
    [] -> Left "No parse"
    [x] -> Right x
    _ -> Left "Ambiguous parse"

parseUnsafe :: Parse a => String -> a
parseUnsafe x = case parseEither x of
    Right x -> x
    Left err -> error err
