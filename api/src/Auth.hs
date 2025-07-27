module Auth
  ( User (..),
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Auth.Server as SAS

newtype User = User
  { username :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON User
instance FromJSON User
instance ToJWT User
instance FromJWT User

-- | This is the user type that will be returned when a user is authenticated.
type instance AuthServerData (Auth auths User) = User
