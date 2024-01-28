{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.StorageTypes where

import           ClassyPrelude              hiding (Handler, singleton, sortBy)
import           Control.Lens               (makeLenses)
import           Data.Aeson                 (FromJSON, Object, ToJSON (toJSON))
import           Data.Aeson.KeyMap          (singleton)
import           Data.Binary                (Binary)
import           Data.Binary.Instances.Time ()
import           Data.Swagger               (ToParamSchema, ToSchema)
import           Database.Persist           (PersistCore (BackendKey),
                                             PersistEntity (Key))
import           Database.Persist.Sql       (SqlBackend)
import           Database.Persist.TH        (MkPersistSettings (mpsGenerateLenses),
                                             mkMigrate, mkPersist,
                                             persistLowerCase, share,
                                             sqlSettings)
import           Katip                      (LogItem (payloadKeys),
                                             PayloadSelection (AllKeys),
                                             ToObject (toObject), Verbosity)
import           Servant                    (FromHttpApiData (parseQueryParam))

data StorageError
  = CommentNotFound
  | UserNotFound
  | ConvoNotFound
  deriving (Eq, Show)

-- | Create Comment obj with persistent to support storage with SQLite & other SQL databases
--   and include lens definitions.
share
  [ mkPersist sqlSettings
      { mpsGenerateLenses = True
      }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
User
  username    Text
  firstName   Text
  lastName    Text

  Primary username
  UniqueUsername username
  deriving Show Read Eq Ord Generic ToJSON FromJSON

Conversation
  convoUrl    Text
  convoTitle  Text

  Primary convoUrl
  UniqueUrl convoUrl
  deriving Show Read Eq Ord Generic ToJSON FromJSON

Comment
  dateCreated UTCTime default=CURRENT_TIME
  parent      CommentId Maybe
  message     Text
  upvotes     Int
  downvotes   Int

  username    Text
  convoUrl    Text

  Foreign User OnDeleteCascade OnUpdateCascade fk_posted_by username
  Foreign Conversation fk_posted_to convoUrl
  deriving Show Read Eq Ord Generic ToJSON FromJSON
|]

deriving instance Generic (Key Conversation)
deriving instance Generic (Key User)
deriving instance Generic (Key Comment)

instance ToSchema (BackendKey SqlBackend)
instance ToSchema (Key Comment)
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
instance Binary (Key Conversation)
instance Binary (Key User)
instance Binary (Key Comment)
instance Binary Comment
instance Binary Conversation
instance Binary User

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

instance ToParamSchema (BackendKey SqlBackend)
instance ToParamSchema (Key Comment)
instance ToParamSchema SortBy
instance FromHttpApiData SortBy where
  parseQueryParam :: Text -> Either Text SortBy
  parseQueryParam = maybe (Left "Invalid sorting method") Right . readMay

mkComment
  :: Text -- ^Username
  -> Text -- ^Conversation url
  -> Text -- ^Message
  -> IO Comment
mkComment username convoUrl msg = do
  currTime <- getCurrentTime
  pure $ Comment currTime Nothing msg 0 0 username convoUrl

data PureStorage = PureStorage {
  _convoStore   :: Map (Key Conversation) Conversation,
  _userStore    :: Map (Key User) User, -- ^ Key is the Username
  _commentStore :: Map CommentId Comment,
  _nextID       :: CommentId
} deriving (Show, Generic)

instance Binary PureStorage

makeLenses ''PureStorage
