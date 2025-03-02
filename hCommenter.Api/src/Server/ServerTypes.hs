{-# LANGUAGE TemplateHaskell #-}

module Server.ServerTypes (ErrorResponse (..), CustomError (..), InputError (..), Backend (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Katip (Scribe)
import Optics

import Database.StorageTypes (StorageError)

data Backend
  = LocalFile
  | SQLite
  | ToBeDeterminedProd
  deriving (Show)

data CustomError
  = StorageError !StorageError
  | InputError !InputError
  deriving (Eq, Show)

data ErrorResponse = ErrorResponse
  { error :: !Text
  , status :: !Int
  }
  deriving (Generic)

instance ToJSON ErrorResponse

instance FromJSON ErrorResponse

newtype InputError = BadArgument Text
  deriving (Eq, Show)
