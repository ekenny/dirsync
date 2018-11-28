module Main where

import Lib
import System.Directory
import Data.ByteString.Lazy.Char8 as BS
import Config
import Data.Aeson
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  conf <- BS.readFile (args !! 0)
  config <- return $ decode conf
  syncConfiguredPaths config
