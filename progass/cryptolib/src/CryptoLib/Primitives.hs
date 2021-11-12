module CryptoLib.Primitives (eea, modExp, modInv, fermatPT) where

import Prelude hiding (gcd)

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Int, Int) -> (Int, Int, Int)
eea (0, b) = (b, 0, 1)
eea (a, b) = let (gcd, x1, y1) = eea(b `mod` a, a)
                 x = y1 - (b `div` a) * x1
                 y = x1
             in (gcd, x, y)


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
