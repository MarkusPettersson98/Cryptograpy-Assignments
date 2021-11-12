{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import qualified Data.ByteString as B (unpack, pack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Hex as H (unhexM)
import Data.Word (Word8)
import Data.Bits (Bits, xor)

main :: IO ()
main = do
  let file = "input.txt"
  content <- readFile file
  (first_block, encrypted) <- parseInput content
  let m = recoverMessage first_block encrypted
  putStrLn $ "Recovered message: " ++ show m

-- | Parses the problem.
parseInput :: MonadFail m => String -> m (Block, Encrypted Block)
parseInput content = do
  let fileLines = lines content
  first_block <- pure . B.unpack . BC.pack $ fileLines !! 0
  encrypted   <- Encrypted . B.unpack <$> (H.unhexM . BC.pack) (fileLines !! 1)
  return (first_block, encrypted)

-- | Recover the encrypted message, knowing the first block of plain text. The
-- encrypted text is of the form C0 | C1 | ... | Cn where each block is 12 bytes
-- long, and C0 = IV.
recoverMessage :: Block -> Encrypted Block -> String
recoverMessage first_block encrypted =
  let Encrypted cs@(iv:c1:_) = chunksOf (12 :: Bytes) <$> encrypted
      -- m           = first_block
      -- c1          = (m ⨁ iv) ⨁ k
      -- Solve for k
      -- k           = c1 ⨁ (m ⨁ iv)
      -- We can construct key by xor-ing iv, c1 and first_block
      key     = iv ⨁ c1 ⨁ first_block
      message = cbcDecryptMessage key (Encrypted iv) (map Encrypted cs)
  in (BC.unpack . B.pack) . (concat . tail) $ message
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf n list
      | n > length list = pure list
      | otherwise = take n list : chunksOf n (drop n list)

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

-- | Decrypt multiple blocks / all blocks of a message at once. Takes as
-- argument a key and an initial vector (IV), and a list of ordered cipher
-- texts. Iterates over all ciphertexts and decrypts them, accumulating the
-- message of each decrypted block.
cbcDecryptMessage :: Key -> Encrypted Block -> [Encrypted Block] -> [Block]
cbcDecryptMessage key iv = accumulate . scanl (cbcRound key) (iv, mempty)
 where
  accumulate :: [(a, b)] -> [b]
  accumulate = tail . map snd

-- | One round of decryption for this simple Block Cipher. Prepares for
-- decryption of next cipher text by providing the current encrypted cipher text
-- as one of the return values.
cbcRound :: Key -> (Encrypted Block, Block) -> Encrypted Block -> (Encrypted Block, Block)
cbcRound key (cprev, _) c = (c, decrypt (key, cprev) c)
