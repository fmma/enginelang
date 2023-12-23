module Main where

import Control.Monad
import System.Environment

import Exp
import qualified Type

example0 = rep $ unlines
    [ "{"
    , "  id x = x;"
    , "}"
    ]

example1 = rep $ unlines
    [ "fib x = x < 2 ? 1 : fib (x - 1) + fib (x - 2);"
    , "fib 10"
    ]

example2 = rep $ unlines
    [ "type FooBar = forall b. {"
    , "  foo : forall a. a => b,"
    , "  bar : forall a. a => b,"
    , "  undefined : forall a. a"
    , "}"
    , ""
    , "fooBar = {"
    , "  foo = bar,"
    , "  bar = foo,"
    , "  undefined = undefined"
    , "} : FooBar"
    ]

rep :: String -> IO ()
rep e = case  runCstr $ typeInference [] (Type.object []) (read e) of
    Right t -> putStrLn (Type.pprint t)
    Left err -> putStrLn err

repFile :: FilePath -> IO ()
repFile = readFile >=> rep

main = do
    [a] <- getArgs
    repFile a
