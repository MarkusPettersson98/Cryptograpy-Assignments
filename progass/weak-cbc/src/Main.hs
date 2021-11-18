import Data.Bits (Bits, xor)
import qualified Data.ByteString as B (pack, unpack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Hex (unhexM)
import Data.Word (Word8)
import Utils

type Block = [Word8]
type Key = [Word8]
type Bytes = Int

main :: IO ()
main = do
  let file = "input.txt"
  content <- readFile file
  (first_block, encrypted) <- parseInput content
  let m = recoverMessage first_block encrypted
  putStrLn $ "Recovered message: " ++ show m

-- | Parses the problem.
parseInput :: MonadFail m => String -> m (Block, [Block])
parseInput content = do
  let (line1 : line2 : _) = lines content
      first_block = B.unpack . BC.pack $ line1
  encrypted <- chunksOf (12 :: Bytes) . B.unpack <$> (unhexM . BC.pack) line2
  return (first_block, encrypted)

-- | Recover the encrypted message, knowing the first block of plain text. The
-- encrypted text is of the form C0 | C1 | ... | Cn where each block is 12 bytes
-- long, and C0 = IV.
--
-- m           = first_block
-- c1          = (m ⨁ iv) ⨁ key
-- Solve for key
-- key         = c1 ⨁ (m ⨁ iv)
-- We can construct key by xor-ing iv, c1 and first_block
recoverMessage :: Block -> [Block] -> String
recoverMessage first_block encrypted@(iv : c1 : _) = BC.unpack . B.pack . concat . cbcDecryptMessage key $ encrypted
  where
    key = iv ⨁ c1 ⨁ first_block

-- * Helper functions for decryption

-- | Decrypt multiple blocks / all blocks of a message at once. Takes as
-- argument a key and a list of ordered cipher texts. Iterates over all
-- ciphertexts and decrypts them, accumulating the message of each decrypted
-- block.
--
-- IV | C1 -> zipWith (decrypt key) => key ⨁ (IV ⨁ C1) = m1
-- C1 | C2 -> zipWith (decrypt key) => key ⨁ (C1 ⨁ C2) = m2
-- C2 | C3 -> zipWith (decrypt key) => key ⨁ (C2 ⨁ C3) = m3
-- C3 | C4 -> zipWith (decrypt key) => key ⨁ (C3 ⨁ C4) = m4
-- .. | .. -> ..
cbcDecryptMessage :: Key -> [Block] -> [Block]
cbcDecryptMessage key blocks = zipWith (decrypt key) blocks (tail blocks)

-- | decrypt key c_{n-1} c_n =>
--  c_n = key ⨁ (c_n-1 ⨁ m_n) =>
--  m_n = key ⨁ (c_n-1 ⨁ c_n) = D_k(c_n)
decrypt :: Key -> Block -> Block -> Block
decrypt k c m = k ⨁ c ⨁ m

-- | xor on the level of [Bits a] instead of Bits a.
(⨁) :: Bits a => [a] -> [a] -> [a]
(⨁) = zipWith xor
