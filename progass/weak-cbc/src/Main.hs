{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
import Control.Monad (liftM2)
import Prelude hiding ((||))
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
  first_block <- pure . B.unpack . BC.pack . head $ fileLines
  encrypted   <- Encrypted . B.unpack <$> (H.unhexM . BC.pack) (fileLines !! 1)
  return (first_block, encrypted)

-- | Recover the encrypted message, knowing the first block of plain text. The
-- encrypted text is of the form IV | C0 | C1 | ... where each block is 12 bytes
-- long.
recoverMessage :: Block -> Encrypted Block -> String
recoverMessage first_block encrypted =
  -- TODO. Decrypt the message on the byte (Word8) representation. When you have
  -- the final message, convert it to a string a shown below.
  let key = undefined -- TODO
      message = decrypt key encrypted
  in BC.unpack . B.pack $ message

-- | Data structure representing something that is encrypted.
newtype Encrypted a = Encrypted a
type Block = [Word8]
type Key = [Word8]

-- | A general class for a Cipher.
class Cipher a where
  -- | Encrypt a message using a key
  encrypt :: Key -> a -> Encrypted a
  -- | Decrypt a message using a key
  decrypt :: Key -> Encrypted a -> a

-- | Define a Block Cipher.
instance Cipher Block where
  -- | encrypt k m = m' = k ⨁ m
  encrypt :: Key -> Block -> Encrypted Block
  encrypt k m = Encrypted (k ⨁ m)
  -- | Encrypted m == m' = k ⨁ m
  -- decrypt k m' == k ⨁ (k ⨁ m) = (k ⨁ k) ⨁ m = {0}^n ⨁ m = m
  --                 ^decrypt        ^associativity
  -- where n = length of key.
  decrypt :: Key -> Encrypted Block -> Block
  decrypt k (Encrypted m) = k ⨁ m

-- | xor on the level of m a (e.g. [a]) instead of a.
(⨁) :: Bits a => [a] -> [a] -> [a]
(⨁) = liftM2 xor

-- | Concatenation of blocks is free, since lists are instances of Semigroup.
(||) :: [a] -> [a] -> [a]
(||) = (<>)
