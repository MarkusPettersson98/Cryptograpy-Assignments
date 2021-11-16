import qualified Data.ByteString as B (unpack, pack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Hex as H (unhexM)
import Cipher
import Utils

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
  in BC.unpack . B.pack . concat . drop 1 $ message

-- | Decrypt multiple blocks / all blocks of a message at once. Takes as
-- argument a key and an initial vector (IV), and a list of ordered cipher
-- texts. Iterates over all ciphertexts and decrypts them, accumulating the
-- message of each decrypted block.
cbcDecryptMessage :: Key -> Encrypted Block -> [Encrypted Block] -> [Block]
cbcDecryptMessage key iv = map snd . tail . scanl (cbcRound key) (iv, mempty)


-- | One round of decryption for this simple Block Cipher. Prepares for
-- decryption of next cipher text by providing the current encrypted cipher text
-- as one of the return values.
cbcRound :: Key -> (Encrypted Block, Block) -> Encrypted Block -> (Encrypted Block, Block)
cbcRound key (cprev, _) c = (c, decrypt (key, cprev) c)
