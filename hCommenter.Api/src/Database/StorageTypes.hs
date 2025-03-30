{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.StorageTypes where

import Data.Aeson
  ( Object
  , ToJSON (toJSON)
  , defaultOptions
  )
import Data.Aeson.KeyMap (singleton)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger
  ( ToParamSchema
  , ToSchema (declareNamedSchema)
  , defaultSchemaOptions
  , genericDeclareNamedSchema
  )
import Database.Persist
  ( PersistCore (BackendKey)
  , PersistEntity (Key)
  )
import Database.Persist.Sql (SqlBackend, toSqlKey)
import Database.Persist.TH
  ( MkPersistSettings (mpsFieldLabelModifier)
  , mkMigrate
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
import Prelude hiding (Handler, singleton, sortBy)

data StorageError
  = CommentNotFound Text
  | UserOrConvoNotFound Text
  | UnhandledStorageError Text
  deriving (Eq, Show)

{-| Create Comment obj with persistent to support storage with SQLite & other SQL databases
  and include lens definitions.
-}
share
  [ mkPersist
      sqlSettings
        { mpsFieldLabelModifier = \_ field -> field
        }
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
User
  username    Text
  firstName   Text
  lastName    Text

  Primary username
  deriving Show Read Eq Generic

Conversation
  url    Text
  title  Text

  Primary url
  deriving Show Read Eq Generic

Comment
  dateCreated UTCTime default=CURRENT_TIME
  message     Text
  upvotes     Int
  downvotes   Int

  parent      CommentId Maybe
  author      Text
  convoUrl    Text

  Foreign Comment fk_parent parent
  Foreign User fk_posted_by author
  Foreign Conversation fk_posted_to convoUrl
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
      { dateCreated = currTime
      , message = comment ^. #message
      , upvotes = 0
      , downvotes = 0
      , parent = toSqlKey <$> comment ^. #parent
      , author = comment ^. #author
      , convoUrl = comment ^. #convoUrl
      }

deriving instance Generic (Key Conversation)

deriving instance Generic (Key User)

deriving instance Generic (Key Comment)

instance ToSchema NewComment where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance FromHttpApiData NewComment where
  parseQueryParam :: Text -> Either Text NewComment
  parseQueryParam = maybe (Left "Comment not formatted correctly") Right . readMay

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
  parseQueryParam = maybe (Left "Invalid sorting method") Right . readMay

mkComment
  :: Text
  -- ^ Username
  -> Text
  -- ^ Conversation url
  -> Text
  -- ^ Message
  -> IO Comment
mkComment username convoUrl msg = do
  currTime <- getCurrentTime
  pure $ Comment currTime msg 0 0 Nothing username convoUrl

data PureStorage = PureStorage
  { convoStore :: Map (Key Conversation) Conversation
  , userStore :: Map (Key User) User
  -- ^ Key is the Username
  , commentStore :: Map (Key Comment) Comment
  , nextID :: CommentId
  }
  deriving (Generic, Show)

makeFieldLabelsNoPrefix ''PureStorage
