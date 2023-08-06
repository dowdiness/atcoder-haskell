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

readInt = fst . fromJust . BS.readInteger
readIntList = map (read @Int . BS.unpack) . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getString = BS.unpack <$> BS.getLine
getStringList = map (read @String . BS.unpack) . BS.words <$> BS.getLine
getCharInputs = BS.words <$> BS.getLine

undef :: Int
undef = -1

main :: IO ()
main = do
  [_, p, q] <- getIntList
  xs <- getIntList

  print $ length $ filter (== q) $ map ((`mod` p) . product) (cb xs 5)

cb :: [Int] -> Int -> [[Int]]
cb _ 0 = [[]]
cb xs n =
  nxs >>= (\(nx, x) -> (x:) <$> cb [z | (m,z) <- nxs, m > nx] (n-1) )
  where
    nxs = zip [1..] xs

-- solve :: [Int] -> Int -> Int -> Int
-- solve xs p q = step 5 xs q
--   where
--     step n (x:xs) acc =
