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

readInt = fst . fromJust . BS.readInteger
readIntList = map (read @Int . BS.unpack) . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getCharInputs = BS.words <$> BS.getLine

main :: IO ()
main = do
  [n, m] <- getIntList
  as <- getCharInputs
  bt <- getCharInputs
  putStrLn $ unlines $ map (\a -> if a then "Yes" else "No") $ solve as bt

solve :: [BS.ByteString] -> [BS.ByteString] -> [Bool]
solve (a:as) bs@(b:bs') | a == b = True : solve as bs'
solve (_ : as) bs       = False : solve as bs
solve _ _               = []
