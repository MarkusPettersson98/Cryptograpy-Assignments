{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)

main = do
  let file = "input.txt"
  content <- readFile file
  let (p, g, y, year, month, day, hour, minute, second, c1, c2) = parseInput content
  let m = recoverMessage p g y year month day hour minute second c1 c2
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- | Parses the problem.
parseInput :: String -> (Integer, Integer, Integer, Int, Int, Int, Int, Int, Int, Integer, Integer)
parseInput content =
  let fileLines = take 6 $ lines content
      p  = readOne (fileLines !! 0)
      g  = readOne (fileLines !! 1)
      y  = readOne (fileLines !! 2)
      t = (T.splitOn "=" (T.pack (fileLines !! 3))) !! 1
      date = (T.splitOn " " t) !! 0
      time = (T.splitOn " " t) !! 1
      year   = (read . T.unpack) $ (T.splitOn "-" date) !! 0
      month  = (read . T.unpack) $ (T.splitOn "-" date) !! 1
      day    = (read . T.unpack) $ (T.splitOn "-" date) !! 2
      hour   = (read . T.unpack) $ (T.splitOn ":" time) !! 0
      minute = (read . T.unpack) $ (T.splitOn ":" time) !! 1
      second = (read . T.unpack) $ (T.splitOn ":" time) !! 2
      c1  = readOne (fileLines !! 4)
      c2  = readOne (fileLines !! 5)
  in  (p, g, y, year, month, day, hour, minute, second, c1, c2)
  where readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1


recoverMessage :: Integer -> Integer -> Integer -> Int -> Int -> Int -> Int ->
                  Int -> Int -> Integer -> Integer -> Integer
recoverMessage p g y year month day hour minute second c1 c2 =
  -- TODO.
  p
