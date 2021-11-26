module Main where

import Test.DocTest
import System.Directory
import System.FilePath ((</>))

main :: IO ()
main = do
  fs <- listDirectory "src"
  doctest $ "-isrc" : map ("src" </>) fs
