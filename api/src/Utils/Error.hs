module Utils.Error where

import Database.Schema (StorageError)
import RestAPI.ServerTypes (InputError)

data CustomError
  = StorageError !StorageError
  | InputError !InputError
  deriving (Eq, Show)
