{-# LANGUAGE DeriveGeneric #-}

module Config  where

import GHC.Generics
import Data.Aeson

data DirMapping = DirMapping {
    source :: FilePath
  , destination  :: FilePath
} deriving (Generic, Show)

instance ToJSON DirMapping where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DirMapping where

data DirSyncConfig = DirSyncConfig [DirMapping]
  deriving (Generic, Show)


instance ToJSON DirSyncConfig where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DirSyncConfig where
