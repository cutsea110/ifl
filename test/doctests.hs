module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/Iseq.hs"
               , "src/Parser.hs"
               , "src/Language.hs"
               , "src/Utils.hs"
               ]
