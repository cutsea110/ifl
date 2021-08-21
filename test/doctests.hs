module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/Parser.hs"
               , "src/Language.hs"
               ]
