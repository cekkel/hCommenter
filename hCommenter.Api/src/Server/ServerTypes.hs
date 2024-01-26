{-# LANGUAGE TemplateHaskell #-}
module Server.ServerTypes (ErrorResponse (..), CustomError (..), InputError (..), Backend (..), Env (Env), backend, appName, envName, scribeName, scribe) where

import           ClassyPrelude
import           Control.Lens          (makeLenses)
import           Data.Aeson            (FromJSON, ToJSON)
import           Database.StorageTypes (StorageError)
import           Katip                 (Scribe)

data Backend
  = LocalFile
  | ToBeDeterminedProd
  deriving (Show)

data Env = Env {
    _backend    :: !Backend
  , _appName    :: !Text
  , _envName    :: !Text
  , _scribeName :: !Text
  , _scribe     :: !Scribe
}
makeLenses ''Env

data CustomError
  = StorageError !StorageError
  | InputError !InputError
  deriving (Eq, Show)

data ErrorResponse = ErrorResponse
  { error  :: !Text
  , status :: !Int
  }
  deriving (Generic)

instance ToJSON ErrorResponse
instance FromJSON ErrorResponse

newtype InputError = BadArgument Text
  deriving (Eq, Show)
