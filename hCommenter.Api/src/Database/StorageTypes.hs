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
User
  username Text
  firstName Text
  lastName Text
  UniqueUsername username

Conversation
  convoTitle Text
  convoUrl Text
  UniqueUrl convoUrl

Comment
  postedBy UserId
  postedTo ConversationId
  message Text
  parent CommentId Maybe
  upvotes Int
  downvotes Int
|]

deriving instance Show User
deriving instance Eq User
deriving instance Ord User
deriving instance Generic User
deriving instance Generic (Key User)
deriving instance ToJSON User
deriving instance FromJSON User

deriving instance Show Conversation
deriving instance Eq Conversation
deriving instance Ord Conversation
deriving instance Generic Conversation
deriving instance Generic (Key Conversation)
deriving instance ToJSON Conversation
deriving instance FromJSON Conversation

deriving instance Show Comment
deriving instance Read Comment
deriving instance Eq Comment
deriving instance Ord Comment
deriving instance Generic Comment
deriving instance Generic (Key Comment)
deriving instance ToJSON Comment
deriving instance FromJSON Comment

instance ToSchema (BackendKey SqlBackend)
instance ToSchema (Key User)
instance ToSchema (Key Conversation)
instance ToSchema (Key Comment)

instance ToParamSchema (BackendKey SqlBackend)
instance ToParamSchema (Key User)
instance ToParamSchema (Key Conversation)
instance ToParamSchema (Key Comment)

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
instance Binary (Key User)
instance Binary (Key Conversation)
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

mkComment :: UserId -> ConversationId -> Text -> Comment
mkComment uID sID msg = Comment uID sID msg Nothing 0 0

data PureStorage = PureStorage {
  _store  :: Map CommentId Comment,
  _nextID :: CommentId
} deriving (Show, Generic)

instance Binary PureStorage

makeLenses ''PureStorage
