module Server.ServerTypes (ErrorResponse (..), CustomError (..), InputError (..)) where

import           ClassyPrelude
import           Data.Aeson            (FromJSON, ToJSON)
import           Database.StorageTypes (StorageError)

data CustomError
  = StorageError StorageError
  | InputError InputError
  deriving (Eq, Show)

data ErrorResponse = ErrorResponse
  { error  :: Text
  , status :: Int
  }
  deriving (Generic)

instance ToJSON ErrorResponse
instance FromJSON ErrorResponse

newtype InputError = BadArgument Text
  deriving (Eq, Show)
