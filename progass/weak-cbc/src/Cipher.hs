{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Cipher where
import Data.Word (Word8)
import Data.Bits (Bits, xor)

-- | Data structure representing something that is encrypted.
newtype Encrypted a = Encrypted a

instance Functor Encrypted where
  fmap f (Encrypted a) = Encrypted (f a)

type Block = [Word8]
type Key   = [Word8]
type Bytes = Int

-- | A general class for a Cipher.
class KeyCipher key a where
  -- | Encrypt a message using a key
  encrypt :: key -> a -> Encrypted a
  -- | Decrypt a message using a key
  decrypt :: key -> Encrypted a -> a

-- | Define a Block Cipher.
-- Implied CBC mode.
instance KeyCipher (Key, Encrypted Block) Block where
  -- | encrypt (k, c_n-1) m_n = k ⨁ (c_n-1 ⨁ m_n) = E_k(m_n)
  encrypt :: (Key, Encrypted Block) -> Block -> Encrypted Block
  encrypt (k, Encrypted c) m = Encrypted (k ⨁ (c ⨁ m))
  -- | decrypt (k, c_n-1) c_n =>
  -- c_n = k ⨁ (c_n-1 ⨁ m_n) =>
  -- m_n = k ⨁ (c_n-1 ⨁ c_n) = D_k(c_n)
  decrypt :: (Key, Encrypted Block) -> Encrypted Block -> Block
  decrypt (k, Encrypted c) (Encrypted m) = k ⨁ (c ⨁ m)

-- * Helper functions for decryption (General + Block Cipher in CBC mode).

-- | xor on the level of [Bits a] instead of Bits a.
(⨁) :: Bits a => [a] -> [a] -> [a]
(⨁) = zipWith xor
