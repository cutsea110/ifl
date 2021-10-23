module Main where

import Template.Mark1 (parse, compile, eval, showResults)

testProg0, testProg1, testProg2 :: String
testProg0 = "main = S K K 3"
testProg1 = "main = S K K"
testProg2 = unlines [ "id = S K K ;"
                    , "main = twice twice twice id 3"
                    ]
testProg3 = unlines [ "pair x y f = f x y ;"
                    , "fst p = p K ;"
                    , "snd p = p K1 ;"
                    , "f x y = letrec"
                    , "           a = pair x b ;"
                    , "           b = pair y a"
                    , "        in"
                    , "        fst (snd (snd (snd a))) ;"
                    , "main = f 3 4"
                    ]

test :: String -> IO ()
test = putStrLn . showResults . eval . compile . parse

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  test testProg3
