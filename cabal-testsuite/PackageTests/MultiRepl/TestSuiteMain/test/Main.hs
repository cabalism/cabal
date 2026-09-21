module Main (main) where

import Control.Monad (unless)
import Pot (boils)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "Watching the pot..."
  unless (boils 100 && not (boils 99)) $ do
    putStrLn "The pot boiled at the wrong temperature!"
    exitFailure
  putStrLn "The watched pot never boils early."
