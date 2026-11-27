module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let actual = reverse "abc"
  unless (actual == "cba") $ do
    putStrLn ("reverse \"abc\" gave " ++ show actual)
    exitFailure
