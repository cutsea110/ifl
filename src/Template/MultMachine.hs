module Template.MultMachine where

-- α変換: 名前の付け替え
-- β簡約: 代入
-- η簡約: \x. P x => P
-- δ簡約: primitive 演算の実行

-- redex : 簡約可能
-- Constant Applicative Form : 
-- Normal Form :
-- Weak Head Normal Form :

-- |
-- n * m => n + n + n + n + ... + n
--          <---------- m -------->
-- d : 
-- t : result
-- init  : d == t == 0
-- final : m == d == 0
type MultState = (Int, Int, Int, Int) -- (n, m, d, t)

evalMult :: MultState -> [MultState]
evalMult state = if multFinal state
  then [state]
  else state:evalMult (stepMult state)

stepMult :: MultState -> MultState
stepMult (n, m, d, t)
  | d >  0 = (n,   m, d-1, t+1) -- 規則1
  | d == 0 = (n, m-1,   n, t  ) -- 規則2

multFinal :: MultState -> Bool
multFinal (_, m, d, _) = m == 0 && d == 0

-- | Exercise 2.1
--
-- (n,m,d,t)
---------------
-- (2,3,0,0)
-- (2,2,2,0)
-- (2,2,1,1)
-- (2,2,0,2)
-- (2,1,2,2)
-- (2,1,1,3)
-- (2,1,0,4)
-- (2,0,2,4)
-- (2,0,1,5)
-- (2,0,0,6)
--

-- | Exercise 2.2
--
-- N*M == n*m+d+t
--
-- 1. N*M == N*M+0+0
-- 2. N*M == n*m+d+t
--   case d == 0
--     N*M == n*m+0+t
--         == n*(m-1)+n+t
--   case d > 0
--     N*M == n*m+d+t
--         == n*m+(d-1)+(t+1)
--
-- 3. N*M == n*0+0+t
--        == t
--
-- 4. (a, b) > (a', b') == a > a' || a == a' && b > b'
--
