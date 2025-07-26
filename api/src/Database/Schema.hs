{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Schema where

import Data.Aeson
  ( Object
  , ToJSON (toJSON)
  , defaultOptions
  )
import Data.Aeson.KeyMap (singleton)
import Data.Aeson.TH (deriveJSON)
import Data.OpenApi
  ( ToParamSchema
  , ToSchema (declareNamedSchema)
  , defaultSchemaOptions
  , genericDeclareNamedSchema
  )
import Data.Time.Clock (getCurrentTime)
import Database.Persist
  ( PersistCore (BackendKey)
  , PersistEntity (Key)
  )
import Database.Persist.Sql (SqlBackend, toSqlKey)
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )
import Katip
  ( LogItem (payloadKeys)
  , PayloadSelection (AllKeys)
  , ToObject (toObject)
  , Verbosity
  )
import Optics
import PyF (PyFCategory (PyFString), PyFClassify)
import Web.HttpApiData

data Backend
  = LocalFile
  | SQLite
  | ToBeDeterminedProd
  deriving (Read, Show)

type instance PyFClassify Backend = 'PyFString

data StorageError
  = CommentNotFound Text
  | UserOrConvoNotFound Text
  | UnhandledStorageError Text
  deriving (Eq, Show)

{-| Create Comment obj with persistent to support storage with SQLite & other SQL databases
  and include lens definitions.
-}
share
  [ mkMigrate "migrateAll"
  , mkPersist sqlSettings
  ]
  [persistLowerCase|
User
  username     Text
  email        Text
  passwordHash Text
  createdAt    UTCTime default=CURRENT_TIME
  updatedAt    UTCTime default=CURRENT_TIME

  Primary username
  UniqueEmail email
  deriving Show Read Eq Generic

Conversation
  url   Text
  title Text

  Primary url
  deriving Show Read Eq Generic

Comment
  conversationId Text
  userId         Text
  text           Text
  upvotes        Int default=0
  downvotes      Int default=0
  parentId       CommentId Maybe
  createdAt      UTCTime default=CURRENT_TIME
  updatedAt      UTCTime default=CURRENT_TIME

  Foreign Conversation fk_conversation conversationId
  Foreign Comment fk_parent parentId
  Foreign User fk_posted_by userId
  deriving Show Read Eq Generic

Moderation
  commentId   CommentId
  moderatorId UserId
  action      Text
  reason      Text Maybe
  createdAt   UTCTime default=CURRENT_TIME

  Foreign Comment fk_comment commentId
  Foreign User fk_moderator moderatorId
  deriving Show Read Eq Generic

Notification
  userId      UserId
  commentId   CommentId
  type        Text
  read        Bool default=False
  createdAt   UTCTime default=CURRENT_TIME

  Foreign User fk_user userId
  Foreign Comment fk_comment commentId
  deriving Show Read Eq Generic
|]

makeFieldLabelsNoPrefix ''Comment
deriveJSON defaultOptions ''Comment

data NewComment = NewComment
  { message :: Text
  , parent :: Maybe Int64
  , author :: Text
  , convoUrl :: Text
  }
  deriving (Eq, Generic, Read, Show)

makeFieldLabelsNoPrefix ''NewComment
deriveJSON defaultOptions ''NewComment

fromNewComment :: NewComment -> IO Comment
fromNewComment comment = do
  currTime <- getCurrentTime
  pure $
    Comment
      { commentCreatedAt = currTime
      , commentUpdatedAt = currTime
      , commentText = comment ^. #message
      , commentUpvotes = 0
      , commentDownvotes = 0
      , commentParentId = toSqlKey <$> comment ^. #parent
      , commentUserId = comment ^. #author
      , commentConversationId = comment ^. #convoUrl
      }

deriving instance Generic (Key Conversation)

deriving instance Generic (Key User)

deriving instance Generic (Key Comment)

instance ToSchema NewComment where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance FromHttpApiData NewComment where
  parseQueryParam :: Text -> Either Text NewComment
  parseQueryParam =
    maybe (Left "Comment not formatted correctly") Right
      . readMay

instance ToObject Comment

data SortBy = Old | New | Popular | Controversial
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

type instance PyFClassify SortBy = 'PyFString

deriveJSON defaultOptions ''SortBy

instance ToObject SortBy where
  toObject :: SortBy -> Object
  toObject = singleton "SortBy" . toJSON

instance LogItem SortBy where
  payloadKeys :: Verbosity -> SortBy -> PayloadSelection
  payloadKeys _ _ = AllKeys

instance ToHttpApiData SortBy where
  toQueryParam = tshow

instance ToParamSchema (BackendKey SqlBackend)

instance ToParamSchema (Key Comment)

instance ToParamSchema SortBy

instance FromHttpApiData SortBy where
  parseQueryParam :: Text -> Either Text SortBy
  parseQueryParam =
    maybe (Left "Invalid sorting method") Right
      . readMay

mkMockComment
  :: Text
  -- ^ Username
  -> Text
  -- ^ Conversation url
  -> Text
  -- ^ Message
  -> IO Comment
mkMockComment username convoUrl msg = do
  currTime <- getCurrentTime
  pure $
    Comment
      { commentConversationId = convoUrl
      , commentUserId = username
      , commentText = msg
      , commentUpvotes = 0
      , commentDownvotes = 0
      , commentParentId = Nothing
      , commentCreatedAt = currTime
      , commentUpdatedAt = currTime
      }

data PureStorage = PureStorage
  { convoStore :: Map (Key Conversation) Conversation
  , userStore :: Map (Key User) User
  -- ^ Key is the Username
  , commentStore :: Map (Key Comment) Comment
  , nextID :: CommentId
  }
  deriving (Generic, Show)

makeFieldLabelsNoPrefix ''PureStorage
