module Main where

-- Convert Int to list of Int
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Convert list of Int to one Int
fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
  where addDigit num d = 10*num + d

undef :: Int
undef = -1
