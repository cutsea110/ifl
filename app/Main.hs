module Main where

-- import Template.Mark1 (parse, compile, eval, showResults)
-- import Template.Mark2 (parse, compile, eval, showResults)
import Template.Mark4 (parse, compile, eval, showResults)

testProg0, testProg1, testProg2, testProg3, testProg4 :: String
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

testProg4 = unlines [ "id x = x ;"
                    , "main = twice twice twice id 3"
                    ]

testProg5 = unlines [ "main = negate (negate 7)"
                    ]

testProg6 = unlines [ "main = ((6 / 2) * 6) + (4 * (10 - 4))"
                    ]

test :: String -> IO ()
test = putStrLn . showResults . eval . compile . parse

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  test testProg6
