module Main where

import Test.DocTest
import System.Directory

main :: IO ()
main = do
  fs <- listDirectory "src"
  doctest $ "-isrc" : map ("src/"++) fs
