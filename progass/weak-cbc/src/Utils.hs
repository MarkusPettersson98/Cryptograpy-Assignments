module Utils where

chunksOf :: Int -> [a] -> [[a]]
chunksOf n list
  | n >= length list = pure list
  | otherwise = chunk : chunksOf n rest
  where (chunk, rest) = splitAt n list
