#!/usr/bin/env stack
{- stack script --resolver lts-18.28 --package array --package bytestring --package containers --package extra --package hashable --package unordered-containers --package heaps --package utility-ht --package vector --package vector-th-unbox --package vector-algorithms --package primitive --package transformers --ghc-options "-D DEBUG" -}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

{-# LANGUAGE BangPatterns        #-}
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
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Typeable
import           Data.Ord

-- #ifdef DEBUG
-- dbg :: Show a => a -> () ; dbg !x = let !_ = traceShow x () in () ; dbgAssert :: Bool -> String -> () ; dbgAssert False !s = error $ "assertion failed!: " ++ s ; dbgAssert True _ = () ;
-- #else
-- dbg :: Show a => a -> () ; dbg _ = () ; dbgAssert :: Bool -> a -> a ; dbgAssert = flip const ;
-- #endif

readInt = fst . fromJust . BS.readInteger
readIntList = map (read @Int . BS.unpack) . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getString = BS.unpack <$> BS.getLine
getStringList = map (read @String . BS.unpack) . BS.words <$> BS.getLine
getCharInputs = BS.words <$> BS.getLine

main :: IO ()
main = do
  [n, k] <- getIntList
  print $ solve n k

solve :: Int -> Int -> Int
solve n 0 = n
solve n 1 = f n
solve n k = solve (f n) (k - 1)

f :: Int -> Int
f x = g1 x - g2 x
  where
    g1 x = fromDigits $ sortBy (comparing Down) $ digs x
    g2 x = fromDigits $ sort $ digs x

-- Convert Int to list of Int
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Convert list of Int to one Int
fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
  where addDigit num d = 10 * num + d
