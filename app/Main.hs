module Main where

import Dirsync
import System.Directory
import Data.ByteString.Lazy.Char8 as BS
import Config
import Data.Aeson
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print $ "Started with args: " ++ show args
  conf <- BS.readFile $ Prelude.head args 
  config <- return $ decode conf
  case config of 
    Nothing -> print "Unable to parse configuration file"
    Just conf -> do
      count <- syncConfiguredPaths conf
      print $ "Complete.  Files Copied: " ++ show count
