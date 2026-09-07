module Main where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.DocTest (doctest)

targetDir :: [FilePath]
targetDir = ["src"]

findHsFiles :: FilePath -> IO [FilePath]
findHsFiles dir = do
  entries <- listDirectory dir
  concat <$> forM entries (\entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then findHsFiles path
      else return [path | takeExtension path == ".hs"])

main :: IO ()
main = do
  files <- concat <$> forM targetDir findHsFiles
  doctest $ map ("-i" ++) targetDir ++ files
