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
getString = BS.unpack <$> BS.getLine
getStringList = map (read @String . BS.unpack) . BS.words <$> BS.getLine

main :: IO ()
main = do
  [n] <- getIntList
  putStrLn $ solve n

solve :: Int -> Int
solve n = n
