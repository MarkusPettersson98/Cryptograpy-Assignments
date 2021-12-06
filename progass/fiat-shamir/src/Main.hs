{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)
import           CryptoLib.Primitives

main = do
  let file = "input.txt"
  input <- parseInput <$> readFile file
  let m = recoverMessage input
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- * Data Types
data Input = Input { n :: Integer
                   , publicX :: Integer
                   , runs :: [Run]
                   }

data Run = Run { r :: Integer
               , c :: Integer
               , s :: Integer
               }

-- | Parses the problem.
parseInput :: String -> Input
parseInput content =
  let fileLines = take 12 $ lines content
      n    = readOne (fileLines !! 0)
      pubX = readOne (fileLines !! 1)
      runs = map parseRun (drop 2 fileLines)
  in Input { n = n, publicX = pubX, runs = runs }
  where
    parseRun :: String -> Run
    parseRun line =
          let elems  = T.splitOn "," (T.pack line) -- R, c, s
              (r:c:s:_) = map (read . T.unpack . (!! 1) . T.splitOn "=") elems -- R=,c=,s=
          in Run r c s
    readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1

-- | Recovers the secret used in this collection of Fiat-Shamir protocol runs.
-- n = the modulus, pubX = the public key, runs = a collection of runs.
-- Each run consists of the three integers [R, c, s].
recoverMessage :: Input -> Integer
recoverMessage (Input n pubX runs) =
  -- TODO. Return x such that x^2 = pubX (mod n).
  n
