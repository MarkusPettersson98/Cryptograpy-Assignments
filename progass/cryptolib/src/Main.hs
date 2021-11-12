module Main where

import CryptoLib.Test

main :: IO ()
main = do
  putStrLn "*** RUNNING TESTSUITE ***"
  runtests
