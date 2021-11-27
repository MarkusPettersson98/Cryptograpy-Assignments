{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Binary           as B (encode)
import qualified Data.ByteString.Char8 as BS (concat, unpack)
import qualified Data.ByteString.Lazy  as BL (toChunks)
import qualified Data.Text             as T (pack, splitOn, unpack)

import           CryptoLib.Primitives  (crt)

main :: IO ()
main = do
  let file = "input.txt"
  content <- readFile file
  let (r1, r2, r3) = parseInput content
  let m = recoverMessage r1 r2 r3
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

-- | Try to recover the message that is encrypted three times under the
-- same public key (e = 3) but different modulus (n).
recoverMessage :: Input -> Input -> Input -> Integer
recoverMessage (Input n1 e1 c1) (Input n2 e2 c2) (Input n3 e3 c3) =
  let (m3, _) = crt [(c1, n1), (c2, n2), (c3, n3)] -- ^ m^3 ∈ Z_{n1 * n2 * n3}
  in cubeRoot m3                                   -- ^ m^3 ∈ Z_{n1 * n2 * n3} < (n1 * n2 * n3) => m = ∛(m^3)

-- * Custom data types

-- | Group all of the data that is to be parsed from `input.txt`.
data Input = Input
  { modulus   :: Integer -- ^ N = p * q
  , publicKey :: Integer
  , cipher    :: Integer -- ^ The encryption of the same message m, aka m^3
  }
  deriving (Show)

-- * Some helper functions

-- | Parses the input of a file. Returns three inputs, representing the modulus,
-- public key and cipher text of each recipient respectively.
parseInput :: String -> (Input, Input, Input)
parseInput content =
  let [i1, i2, i3] = fmap parseLine . take 3 . lines $ content
  in (i1, i2, i3)
  where parseLine line =
          let elems   = T.splitOn "," (T.pack line)
              toInput = \[n, e, c] -> Input { modulus=n, publicKey=e, cipher=c }
              values  =  toInput . map (read . T.unpack . (!! 1) . T.splitOn "=") $ elems
          in values

-- | Turn a (large) integer value into some String representation.
decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

-- | Computes the downwards rounded integer cube root of its argument, which
-- must be positive. Not optimized; performs a binary search.
cubeRoot :: Integer -> Integer
cubeRoot n = binS 1 n
   where binS a b
            | a < b-1   = if m^3 > n then binS a m else binS m b
            | otherwise = a
            where m     = (a + b) `div` 2
