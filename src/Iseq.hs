module Iseq
  ( Iseq (..)
  , IseqRep ()
  , iConcat
  , iInterleave
  , iParen
  , iSpace
  , iNum
  , iFWNum
  , iLayn
  , flatten
  ) where

import Utils (space)

-- | 抽象データ型的に使う
class Iseq iseq where
  iNil     :: iseq
  iStr     :: String -> iseq
  iAppend  :: iseq -> iseq -> iseq
  iNewline :: iseq
  iIndent  :: iseq -> iseq
  iDisplay :: iseq -> String

infixr 5 `iAppend`

-- | Iseq 抽象データ型の具体型
data IseqRep = INil
             | IStr String
             | IAppend IseqRep IseqRep
             | IIndent IseqRep
             | INewline
             deriving Show

instance Iseq IseqRep where
  iNil              = INil
  iStr []           = INil
  iStr s            = case break (=='\n') s of
                        (x, [])  -> IStr s
                        (x, _:y) -> IStr x `iAppend` INewline `iAppend` iStr y
  iAppend INil seq  = seq
  iAppend seq  INil = seq
  iAppend seq1 seq2 = IAppend seq1 seq2
  iNewline          = INewline
  iIndent seq       = IIndent seq
  iDisplay seq      = flatten 0 [(seq, 0)]

flatten :: Int -> [(IseqRep, Int)] -> String
flatten col []                                 = ""
flatten col ((INil, indent):seqs)              = flatten col seqs
flatten col ((IStr s, indent):seqs)            = s ++ flatten (col + length s) seqs
flatten col ((IAppend seq1 seq2, indent):seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent):seqs)          = '\n' : (space indent ++ flatten indent seqs)
flatten col ((IIndent seq, indent):seqs)       = flatten col ((seq, col) : seqs)

iNum :: Iseq iseq => Int -> iseq
iNum n = iStr (show n)

-- | Fix Width number
iFWNum :: Iseq iseq => Int -> Int -> iseq
iFWNum width n = iStr (space (width - length digits) ++ digits)
  where digits = show n

-- | Layout numbers
iLayn :: Iseq iseq => [iseq] -> iseq
iLayn seqs = iConcat (zipWith layItem [1..] seqs)
  where layItem n seq = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

iParen :: Iseq iseq => iseq -> iseq
iParen seq = iStr "(" `iAppend` seq `iAppend` iStr ")"

iSpace :: Iseq iseq => iseq
iSpace = iStr " "

iConcat :: Iseq iseq => [iseq] -> iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq iseq => iseq -> [iseq] -> iseq
iInterleave sep []     = iNil
iInterleave sep [x]    = x
iInterleave sep (x:xs) = iConcat [x, sep, iInterleave sep xs]
