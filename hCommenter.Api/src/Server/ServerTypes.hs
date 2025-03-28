module Server.ServerTypes (ErrorResponse (..), CustomError (..), InputError (..), Backend (..)) where

import Data.Aeson (FromJSON, ToJSON)
import PyF (PyFCategory (PyFString), PyFClassify)

import Database.StorageTypes (StorageError)

data Backend
  = LocalFile
  | SQLite
  | ToBeDeterminedProd
  deriving (Read, Show)

type instance PyFClassify Backend = 'PyFString

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
