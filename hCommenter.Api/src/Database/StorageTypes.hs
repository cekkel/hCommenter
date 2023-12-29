{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.StorageTypes where

import           ClassyPrelude        hiding (Handler, singleton, sortBy)
import           Control.Lens         (makeLenses)
import           Data.Aeson           (FromJSON, Object, ToJSON (toJSON))
import           Data.Aeson.KeyMap    (singleton)
import           Data.Binary          (Binary)
import           Data.Swagger         (ToParamSchema, ToSchema)
import           Database.Persist     (PersistCore (BackendKey),
                                       PersistEntity (Key))
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.TH  (MkPersistSettings (mpsGenerateLenses, mpsPrefixFields),
                                       mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           Katip                (LogItem (payloadKeys),
                                       PayloadSelection (AllKeys),
                                       ToObject (toObject), Verbosity)
import           Servant              (FromHttpApiData (parseQueryParam))

data StorageError = CommentNotFound
  deriving (Eq, Show)

-- | Create Comment obj with persistent to support storage with SQLite & other SQL databases
--   and include lens definitions.
share
  [ mkPersist sqlSettings { mpsGenerateLenses = True, mpsPrefixFields = False }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
Comment
  message Text
  replies [CommentId]
  upvotes Int
  downvotes Int
|]

deriving instance Show Comment
deriving instance Read Comment
deriving instance Generic Comment
deriving instance Generic (Key Comment)
deriving instance ToJSON Comment

instance ToSchema (BackendKey SqlBackend)
instance ToSchema (Key Comment)

instance ToObject CommentId where
  toObject :: CommentId -> Object
  toObject = singleton "ID" . toJSON
instance LogItem CommentId where
  payloadKeys _ _ = AllKeys

instance ToObject [CommentId]
instance LogItem [CommentId] where
  payloadKeys _ _ = AllKeys

instance ToSchema Comment
instance FromHttpApiData Comment where
  parseQueryParam :: Text -> Either Text Comment
  parseQueryParam = maybe (Left "Comment not formatted correctly") Right . readMay

instance ToObject Comment
instance LogItem Comment where
  payloadKeys _ _ = AllKeys

instance ToObject [Comment]
instance LogItem [Comment] where
  payloadKeys _ _ = AllKeys

instance Binary (BackendKey SqlBackend)
instance Binary (Key Comment)
instance Binary Comment

data SortBy = Old | New | Popular | Controversial
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON SortBy
instance FromJSON SortBy

instance ToObject SortBy where
  toObject :: SortBy -> Object
  toObject = singleton "SortBy" . toJSON

instance LogItem SortBy where
  payloadKeys :: Verbosity -> SortBy -> PayloadSelection
  payloadKeys _ _ = AllKeys

instance ToParamSchema SortBy
instance FromHttpApiData SortBy where
  parseQueryParam :: Text -> Either Text SortBy
  parseQueryParam = maybe (Left "Invalid sorting method") Right . readMay

emptyComment :: Comment
emptyComment = mkComment ""

mkComment :: Text -> Comment
mkComment msg = Comment msg [] 0 0

data PureStorage = PureStorage {
  _store  :: Map CommentId Comment,
  _nextID :: CommentId
} deriving (Read, Show, Generic)

instance Binary PureStorage

makeLenses ''PureStorage
