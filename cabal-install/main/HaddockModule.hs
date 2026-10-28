module Main (main) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Environment (getArgs)

import Distribution.Client.CmdHaddockModule (haddockModuleMain)

main :: IO ()
main = getArgs >>= haddockModuleMain
