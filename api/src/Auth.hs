{-# LANGUAGE DeriveAnyClass #-}

module Auth
  ( User (..)
  )
where

import Data.Aeson
import Servant.Auth.Server as SAS
import Servant.Server.Experimental.Auth (AuthServerData)

newtype User = User
  { username :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance ToJSON User

instance FromJSON User

instance ToJWT User

instance FromJWT User

-- | This is the user type that will be returned when a user is authenticated.
type instance AuthServerData (Auth auths User) = User
