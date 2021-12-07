{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (unpack, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import qualified Data.Text as T (splitOn, pack, unpack)
import           CryptoLib.Primitives
import           Data.List (find)

main = do
  let file = "input.txt"
  input <- parseInput <$> readFile file
  case recoverMessage input of
    Nothing -> putStrLn "Failed to recover message."
    Just m -> do
      putStrLn $ "Recovered message: " ++ show m
      putStrLn $ "Decoded message: " ++ decode m

-- | Recovers the secret used in this collection of Fiat-Shamir protocol runs.
-- n = the modulus, pubX = the public key, runs = a collection of runs.
-- Each run consists of the three integers [R, b, z].
recoverMessage :: Input -> Maybe Integer
recoverMessage (Input n pubX runs) = do
  let exchanges = [ (m1, m2) | m1 <- runs, m2 <- runs, b m1 == 0, b m2 == 1]
  -- Find the two messages m_1, m_2 such that (m_1: b=0, m_2 : b_1) && (m_1: Z=r, m_2: Z=r*x) => (((z m_1)^-1 * z m_2 )^2) mod n == X mod n
  (m1, m2) <- find (uncurry makesSecret) exchanges
  pure (x m1 m2 `mod` n)
  where
    x m1 m2 = (z m1 `modInv'` n) * z m2
    makesSecret m1 m2 = (x m1 m2)^2 `mod` n == pubX

decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- * Data Types
data Input = Input { n       :: Integer -- N = p * q', which is a public key.
                   , publicX :: Integer -- X = x^2 mod N, where x is someone's private key.
                   , runs    :: [Run]
                   }

data Run = Run { r :: Integer -- Prover's commitment `R` is same in some case.
               , b :: Integer
               , z :: Integer
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
              (r:b:z:_) = map (read . T.unpack . (!! 1) . T.splitOn "=") elems -- R=,c=,s=
          in Run r b z
    readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1
