module CryptoLib.Primitives where

import Prelude hiding (gcd)
import Control.Monad (foldM)

-- | Returns a triple (gcd, s, t) such that gcd is the greatest common divisor
-- of a and b, and gcd = a*s + b*t.
eea :: (Int, Int) -> (Int, Int, Int)
eea = uncurry eea'

-- | Returns a^k (mod n).
modExp :: (Int, Int, Int) -> Int
modExp (n, a, k) = modExp' n a k

-- | Returns the value v such that a*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv :: (Int, Int) -> Int
modInv = uncurry modInv'

-- | Returns 0 if n is a Fermat Prime, otherwise it returns the lowest
-- Fermat Witness. Tests values from 2 (inclusive) to n/3 (exclusive).
fermatPT :: Int -> Int
fermatPT p = case isPrime p of
               Right _ -> 0 -- ^ p is believed to be a prime number.
               Left witness -> witness
  where
    isPrime :: Int -> Either Witness Prime
    isPrime p = foldM (\_ n -> test p n) 0 (tests p)

    tests :: Int -> [Int]
    tests n = 2 : takeWhile (< n `div` 3) (map succ (tests n))

    test :: Prime -> Int -> Either Witness Prime
    test p a = let x = modExp (p, a, p-1)
               in if x == 1
                  then Right p -- ^ If True, then p is probably a prime.
                  else Left a -- ^ If False, then p is a composite (not prime!). Return witness.

type Witness = Int
type Prime = Int

-- * 'Unbounded', curried versions of the required primitives. These will be
-- * more useful in upcomming assignments.

-- | Returns a^k (mod n).
modExp' :: Integral a => a -> a -> a -> a
modExp' n a 0 = 1
modExp' n a k
  | even k = r
  | otherwise = (r * a) `mod` n
  where
    r = modExp' n ((a * a) `mod` n) (k `div` 2)

-- | Returns the value v such that a*v = 1 (mod m).
-- Returns 0 if the modular inverse does not exist.
modInv' :: Integral a => a -> a -> a
modInv' a m
  | gcd == 1 = x `mod` m
  | gcd == -1 = (-x) `mod` m
  | otherwise = 0
  where
    (gcd, x, _y) = eea' a m

eea' :: Integral a => a -> a -> (a, a, a)
eea' 0 b = (b, 0, 1)
eea' a b =
  let (gcd, x1, y1) = eea' (b `mod` a) a
      x = y1 - (b `div` a) * x1
      y = x1
   in (gcd, x, y)
