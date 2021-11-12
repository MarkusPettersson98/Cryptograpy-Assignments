module CryptoLib.Primitives (eea, modExp, modInv, fermatPT) where

import Prelude hiding (gcd)

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Int, Int) -> (Int, Int, Int)
eea = undefined

-- | Returns a^k (mod n).
modExp :: (Int, Int, Int) -> Int
modExp (n, a, k) = -1

-- | Returns the value v such that n*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv :: (Int, Int) -> Int
modInv (n, m) = -1

-- | Returns 0 if n is a Fermat Prime, otherwise it returns the lowest
-- Fermat Witness. Tests values from 2 (inclusive) to n/3 (exclusive).
fermatPT :: Int -> Int
fermatPT n = -1
