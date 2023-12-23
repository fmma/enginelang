import EngineMain

import Parser

main :: IO ()
main = do
    ex <- readFile "test/example.enl"
    putStrLn $ columnize ex
    libMain
