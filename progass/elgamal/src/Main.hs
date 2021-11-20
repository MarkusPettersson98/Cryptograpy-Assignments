{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)

import CryptoLib.Primitives

main = do
  m <- recoverMessage . parseInput <$> readFile "input.txt"
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

recoverMessage :: Input -> Integer
recoverMessage (Input p g y year month day hour minute second msg) =
  -- TODO.
  p

-- * Custom data types

-- | Define all of the data that is to be parsed from `input.txt`.
data Input = Input { p :: Integer
                   , g :: Integer
                   , y :: Integer

                   , year :: Int
                   , month :: Int
                   , day :: Int
                   , hour :: Int
                   , minute :: Int
                   , second :: Int

                   , message :: Message
                   }

data Message = Message { pubkey :: Integer
                       , cipher :: Integer
                       }


-- * Some helper functions

-- | Parse the input to the problem.
parseInput :: String -> Input
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
      msg = Message { pubkey = c1, cipher = c2 }
  in Input p g y year month day hour minute second msg
  where readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1

-- | Turn a (large) integer value into some String representation.
decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))
