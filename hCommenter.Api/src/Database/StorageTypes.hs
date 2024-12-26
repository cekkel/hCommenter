{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.StorageTypes where

import ClassyPrelude hiding (Handler, singleton, sortBy)
import Control.Lens (makeLenses, (^.))
import Data.Aeson
  ( FromJSON
  , Object
  , ToJSON (toJSON)
  , defaultOptions
  )
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap (singleton)
import Data.Aeson.TH (deriveJSON)
import Data.Binary (Binary)
import Data.Binary.Instances.Time ()
import Data.Swagger
  ( ToParamSchema
  , ToSchema (declareNamedSchema)
  , defaultSchemaOptions
  , genericDeclareNamedSchema
  )
import Data.Swagger.Schema qualified as Schema
import Database.Persist
  ( PersistCore (BackendKey)
  , PersistEntity (Key)
  )
import Database.Persist.Sql (SqlBackend)
import Database.Persist.TH
  ( MkPersistSettings (mpsFieldLabelModifier, mpsGenerateLenses)
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
import Servant (FromHttpApiData (parseQueryParam))

data StorageError
  = CommentNotFound
  | UserNotFound
  | ConvoNotFound
  deriving (Eq, Show)

{- | Create Comment obj with persistent to support storage with SQLite & other SQL databases
  and include lens definitions.
-}
share
  [ mkPersist
      sqlSettings
        { mpsGenerateLenses = True
        , mpsFieldLabelModifier = \_ field -> field
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

-- Ignore leading underscore that's introduced for lens.
deriveJSON defaultOptions{JSON.fieldLabelModifier = drop 1} ''Comment

data NewComment = NewComment
  { _new_message :: Text
  , _new_parent :: Maybe (Key Comment)
  , _new_author :: Text
  , _new_convoUrl :: Text
  }
  deriving (Show, Read, Eq, Generic)

makeLenses ''NewComment
deriveJSON defaultOptions{JSON.fieldLabelModifier = drop 4} ''NewComment

fromNewComment :: NewComment -> IO Comment
fromNewComment comment = do
  currTime <- getCurrentTime
  pure $
    Comment
      { _dateCreated = currTime
      , _message = comment ^. new_message
      , _upvotes = 0
      , _downvotes = 0
      , _parent = comment ^. new_parent
      , _author = comment ^. new_author
      , _convoUrl = comment ^. new_convoUrl
      }

deriving instance Generic (Key Conversation)

deriving instance Generic (Key User)

deriving instance Generic (Key Comment)

instance ToSchema (BackendKey SqlBackend)

instance ToSchema (Key Comment)

instance ToSchema Comment where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions{Schema.fieldLabelModifier = drop 1}

instance ToSchema NewComment where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions{Schema.fieldLabelModifier = drop 5}

instance FromHttpApiData NewComment where
  parseQueryParam :: Text -> Either Text NewComment
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

mkComment ::
  -- | Username
  Text ->
  -- | Conversation url
  Text ->
  -- | Message
  Text ->
  IO Comment
mkComment username convoUrl msg = do
  currTime <- getCurrentTime
  pure $ Comment currTime msg 0 0 Nothing username convoUrl

data PureStorage = PureStorage
  { _convoStore :: Map (Key Conversation) Conversation
  , _userStore :: Map (Key User) User
  -- ^ Key is the Username
  , _commentStore :: Map CommentId Comment
  , _nextID :: CommentId
  }
  deriving (Show, Generic)

instance Binary PureStorage

makeLenses ''PureStorage
