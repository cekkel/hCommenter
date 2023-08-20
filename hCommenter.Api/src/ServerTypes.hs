module ServerTypes (Error (..)) where

import           ClassyPrelude
import           Data.Aeson    (FromJSON, ToJSON)

data Error = Error
  { error  :: Text
  , status :: Int
  }
  deriving (Generic)

instance ToJSON Error
instance FromJSON Error
