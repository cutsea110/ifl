module Utils
  ( space
  ) where

{- |
>>> space 0
""

>>> space 4
"    "
-}
space :: Int -> String
space n = replicate n ' '
