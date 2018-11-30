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
  print ("Started with args: ", args)
  conf <- BS.readFile $ Prelude.head args 
  config <- return $ decode conf
  count <- syncConfiguredPaths config
  print $ "Complete.  Files Copied: " ++ show count
