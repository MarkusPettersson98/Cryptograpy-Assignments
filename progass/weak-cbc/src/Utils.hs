module Utils where

import qualified Data.ByteString as B (pack, unpack)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Hex (unhexM)
import Data.Word (Word8)

-- * Some convenient type aliases.
type Block = [Word8]
type Key = [Word8]
type Bytes = Int

-- | Parses the problem.
parseInput :: MonadFail m => Int -> String -> m (Block, [Block])
parseInput blocksize content = do
  let (line1 : line2 : _) = lines content
      first_block = B.unpack . BC.pack $ line1
  encrypted <- chunksOf blocksize . B.unpack <$> (unhexM . BC.pack) line2
  return (first_block, encrypted)

-- | Split a list into sublists (chunks) of length n,
chunksOf :: Int -> [a] -> [[a]]
chunksOf n list
  | n >= length list = pure list
  | otherwise = chunk : chunksOf n rest
  where (chunk, rest) = splitAt n list
