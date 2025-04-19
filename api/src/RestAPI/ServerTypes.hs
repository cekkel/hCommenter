module RestAPI.ServerTypes (ErrorResponse (..), InputError (..)) where

import Data.Aeson (FromJSON, ToJSON)

data ErrorResponse = ErrorResponse
  { error :: !Text
  , status :: !Int
  }
  deriving (Generic)

instance ToJSON ErrorResponse

instance FromJSON ErrorResponse

newtype InputError = BadArgument Text
  deriving (Eq, Show)
