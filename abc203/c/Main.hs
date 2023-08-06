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

import qualified Control.Monad as CM
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
  [n, k] <- getIntList
  m <- getIntTimes n
  print $ solve (sort m) k 0

solve :: [(Int, Int)] -> Int -> Int -> Int
solve [] k l = k + l
solve ((x, y):ms) k l
  | (k + l - x) < 0 = k + l
  | otherwise       = solve ms y (k + l)

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
getIntMatrix :: IO [[Int]]
getIntMatrix = map readIntList . BS.lines <$> BS.getContents
getIntTimes :: Int -> IO [(Int, Int)]
getIntTimes n = CM.replicateM n $ do
      [w,v] <- getIntList
      return (w,v)
