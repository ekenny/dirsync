module Main where

import Lib
import System.Directory
import Data.ByteString.Lazy.Char8 as BS
import Config
import Data.Aeson

main :: IO ()
main = do
  conf <- BS.readFile "dirsync-test.conf"
  config <- return $ decode conf
  syncConfiguredPaths config
