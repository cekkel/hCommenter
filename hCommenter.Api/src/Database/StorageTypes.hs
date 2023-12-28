{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Database.StorageTypes (mkComment, Comment, replies, message, upvotes, downvotes, ID (ID), mkID, SortBy(..), emptyComment, StorageError (..)) where

import           ClassyPrelude     hiding (Handler, singleton, sortBy)
import           Control.Lens      (makeLenses)
import           Data.Aeson        (FromJSON, Object, ToJSON (toJSON),
                                    defaultOptions)
import           Data.Aeson.KeyMap (singleton)
import           Data.Aeson.TH     (deriveJSON, fieldLabelModifier)
import           Data.Swagger      (ToParamSchema, ToSchema)
import           Katip             (LogItem (payloadKeys),
                                    PayloadSelection (AllKeys),
                                    ToObject (toObject), Verbosity)
import           Servant           (FromHttpApiData (parseQueryParam))

data StorageError = CommentNotFound
  deriving (Eq, Show)

newtype ID = ID Int
  deriving newtype (Show, Read, Eq, Ord, Num, ToSchema, ToParamSchema, FromHttpApiData, ToJSON, FromJSON)

instance ToObject ID where
  toObject :: ID -> Object
  toObject (ID val) = singleton "ID" (toJSON val)
instance LogItem ID where
  payloadKeys _ _ = AllKeys

instance ToObject [ID]
instance LogItem [ID] where
  payloadKeys _ _ = AllKeys

mkID :: Int -> ID
mkID = ID

data Comment = Comment
  { _message   :: Text
  , _replies   :: [ID]
  , _upvotes   :: Int
  , _downvotes :: Int
  } deriving (Show, Read, Generic)

makeLenses ''Comment
-- field label modifier used for compatibility with lens '_' syntax
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Comment

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
