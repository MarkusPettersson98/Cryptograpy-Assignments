{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Fail
import qualified Data.Binary as B (encode)
import qualified Data.ByteString.Char8 as BS (concat, unpack)
import qualified Data.ByteString.Lazy as BL (toChunks)
import Data.Either
import Data.Foldable (find)
import Data.Function ((&))
import qualified Data.Text as T (pack, splitOn, unpack)

import CryptoLib.Primitives

main :: IO ()
main = do
  content <- readFile "input.txt"
  input   <- parseInput content
  m       <- either fail pure $ recoverMessage input
  putStrLn $ "Recovered message: " ++ show m
  putStrLn $ "Decoded message: " ++ decode m

-- | Recover an encrypted message by exploiting some information obtained during
-- eavesdropping.
recoverMessage :: Input -> Either Error Integer
recoverMessage (Input params genInput msg) = decrypt ciphertext <$> recoverKey searchspace
  where
    (SystemParameters prime _ receiverPubKey) = params
    ciphertext = cipher msg
    searchspace = prg genInput
    -- * Some helper functions
    findSecretKey = bruteForce params (senderPubKey msg)
    genK          = \secretKey -> modExp' prime receiverPubKey secretKey -- K = X^{y} mod p = receiverPubKey^{SecretKey (of sender)} mod p
    recoverKey    = \ss -> genK <$> findSecretKey ss
    decrypt       = \c k -> (c * modInv' k prime) `mod` prime -- message = C / K = C * (1/K)

-- | Bruteforce search to find secret key that sender used to encrypt the
-- message with it.
--
-- senderPubKey == g^x mod p, where x∊searchSpace
bruteForce :: SystemParameters -> Integer -> [Integer] -> Either Error Integer
bruteForce (SystemParameters p g _) senderPubKey searchspace =
  find (\x -> modExp' p g x == senderPubKey) searchspace
    & note "Exhausted search space. No potential key was found."

-- | Given some generator input, generate all possibly generated number (the
-- only thing missing is the millisecond component).
prg :: GenInput -> [Integer]
prg (GenInput year month day hour minute second) =
  let base = toInteger $ (year * (10 ^ 10)) + (month * (10 ^ 8)) + (day * (10 ^ 6)) + (hour * (10 ^ 4) + (minute * (10 ^ 2)) + second)
   in [base + millisecond | millisecond <- [0 .. 999]]

-- * Custom data types

-- | Define all of the data that is to be parsed from `input.txt`.
data Input = Input
  { params :: SystemParameters,
    genInput :: GenInput,
    message :: Message
  }
  deriving (Show)

data SystemParameters = SystemParameters
  { p :: Integer,
    g :: Integer,
    y :: Integer
  }
  deriving (Show)

data GenInput = GenInput
  { year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int,
    second :: Int
  }
  deriving (Show)

data Message = Message
  { senderPubKey :: Integer,
    cipher :: Integer
  }
  deriving (Show)

type Error = String

-- * Some helper functions

-- | Parse the input to the problem.
parseInput :: MonadFail m => String -> m Input
parseInput content =
  let fileLines = take 6 $ lines content
      p = readOne (fileLines !! 0)
      g = readOne (fileLines !! 1)
      y = readOne (fileLines !! 2)
      t = (T.splitOn "=" (T.pack (fileLines !! 3))) !! 1
      date = (T.splitOn " " t) !! 0
      time = (T.splitOn " " t) !! 1
      year = (read . T.unpack) $ (T.splitOn "-" date) !! 0
      month = (read . T.unpack) $ (T.splitOn "-" date) !! 1
      day = (read . T.unpack) $ (T.splitOn "-" date) !! 2
      hour = (read . T.unpack) $ (T.splitOn ":" time) !! 0
      minute = (read . T.unpack) $ (T.splitOn ":" time) !! 1
      second = (read . T.unpack) $ (T.splitOn ":" time) !! 2

      params = SystemParameters {p = p, g = g, y = y}
      genInput = GenInput {year = year, month = month, day = day, hour = hour, minute = minute, second = second}
      c1 = readOne (fileLines !! 4)
      c2 = readOne (fileLines !! 5)
      msg = Message {senderPubKey = c1, cipher = c2}
   in pure $ Input params genInput msg
  where
    readOne line = (read . T.unpack) $ (T.splitOn "=" (T.pack line)) !! 1

-- | Turn a (large) integer value into some String representation.
decode :: Integer -> String
decode val = reverse (BS.unpack ((BS.concat . BL.toChunks) $ B.encode val))

note :: a -> Maybe b -> Either a b
note a Nothing = Left a
note a (Just b) = Right b
