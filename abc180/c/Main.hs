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

-- #ifdef DEBUG
-- dbg :: Show a => a -> () ; dbg !x = let !_ = traceShow x () in () ; dbgAssert :: Bool -> String -> () ; dbgAssert False !s = error $ "assertion failed!: " ++ s ; dbgAssert True _ = () ;
-- #else
-- dbg :: Show a => a -> () ; dbg _ = () ; dbgAssert :: Bool -> a -> a ; dbgAssert = flip const ;
-- #endif

main :: IO ()
main = do
  n <- getInt
  mapM_ print $ sort $ solve n 1 []

solve :: Integer -> Integer -> [Integer] -> [Integer]
solve n m acc
  | m ^ (2 :: Integer) > n = acc
  | n `mod` m == 0          = solve n (m + 1) (dividable n m) ++ acc
  | otherwise               = solve n (m + 1) acc
  where
    dividable i j
      | j /= (i `div` j) = [j, i `div` j]
      | otherwise      = [j]

-- Liblary

readInt :: BS.ByteString -> Integer
readInt = fst . fromJust . BS.readInteger
readIntList :: BS.ByteString -> [Int]
readIntList = map (read @Int . BS.unpack) . BS.words

getInt :: IO Integer
getInt = readInt <$> BS.getLine
getIntList :: IO [Int]
getIntList = readIntList <$> BS.getLine
getString :: IO [Char]
getString = BS.unpack <$> BS.getLine
getStringList :: IO [String]
getStringList = map (read @String . BS.unpack) . BS.words <$> BS.getLine
getCharInputs :: IO [BS.ByteString]
getCharInputs = BS.words <$> BS.getLine
