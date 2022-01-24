{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import           Data.Typeable

readInt = fst . fromJust . BS.readInteger
readIntList = map (read @Int . BS.unpack) . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getString = BS.unpack <$> BS.getLine
getStringList = map (read @String . BS.unpack) . BS.words <$> BS.getLine

main :: IO ()
main = do
  s <- getString
  [a, b] <- getIntList
  putStrLn $ solve (a - 1) (b - 1) s

solve :: Int -> Int -> String -> String
solve 0 b ss'@(s:ss) = ss' !! b : take (b-1) ss ++ s : drop b ss
solve a b (s:ss)     = s : solve (a - 1) (b - 1) ss
