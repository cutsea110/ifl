module Main where

import Template.Mark1 (parse, compile, eval, showResults)

testProg0, testProg1, testProg2 :: String
testProg0 = "main = S K K 3"
testProg1 = "main = S K K"
testProg2 = unlines [ "id = S K K ;"
                    , "main = twice twice twice id 3"
                    ]

test :: String -> IO ()
test = putStrLn . showResults . eval . compile . parse

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  test testProg2
