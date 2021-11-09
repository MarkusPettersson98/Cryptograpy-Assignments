import qualified Data.ByteString as B (unpack, pack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Hex as H (unhexM)
import Data.Word (Word8)

main :: IO ()
main = do
  let file = "input.txt"
  content <- readFile file
  (first_block, encrypted) <- parseInput content
  let m = recoverMessage first_block encrypted
  putStrLn $ "Recovered message: " ++ show m

-- | Parses the problem.
parseInput :: MonadFail m => String -> m ([Word8], [Word8])
parseInput content = do
  let fileLines = lines content
  first_block <- pure . B.unpack . BC.pack . head $ fileLines
  encrypted   <- B.unpack <$> (H.unhexM . BC.pack) (fileLines !! 1)
  return (first_block, encrypted)

-- | Recover the encrypted message, knowing the first block of plain text. The
-- encrypted text is of the form IV | C0 | C1 | ... where each block is 12 bytes
-- long.
recoverMessage :: [Word8] -> [Word8] -> String
recoverMessage first_block encrypted =
  -- TODO. Decrypt the message on the byte (Word8) representation. When you have
  -- the final message, convert it to a string a shown below.
  BC.unpack (B.pack encrypted)
