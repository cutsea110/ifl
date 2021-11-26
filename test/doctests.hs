module Main where

import Test.DocTest
import System.Directory
import System.FilePath ((</>))

srcDir :: FilePath
srcDir = "src"

main :: IO ()
main = do
  fs <- listDirectory srcDir
  doctest $ "-isrc" : map (srcDir </>) fs
