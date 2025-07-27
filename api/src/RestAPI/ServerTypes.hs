module RestAPI.ServerTypes (ErrorResponse (..), InputError (..), ApiContexts) where

import Data.Aeson (FromJSON, ToJSON)
import Servant (ErrorFormatters)
import Servant.Auth.Server (CookieSettings, JWTSettings)

data ErrorResponse = ErrorResponse
  { error :: !Text
  , status :: !Int
  }
  deriving (Generic)

instance ToJSON ErrorResponse

instance FromJSON ErrorResponse

data InputError
  = BadArgument Text
  | AuthError Text
  deriving (Eq, Show)

type ApiContexts = '[ErrorFormatters, CookieSettings, JWTSettings]
