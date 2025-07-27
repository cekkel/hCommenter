module Mapping.Typeclass where

import Database.Persist.Sql (Entity (Entity))

import Mapping.Comments (storageToViewComment)
import Mapping.ExternalTypes (ViewComment, ViewUser)
import Mapping.Users (storageToViewUser)

import Database.Schema qualified as Schema

class MapsFrom a b where
  mapFrom :: a -> b

instance MapsFrom (Entity Schema.Comment) ViewComment where
  mapFrom :: Entity Schema.Comment -> ViewComment
  mapFrom (Entity key comment) = storageToViewComment key comment

instance MapsFrom (Entity Schema.User) ViewUser where
  mapFrom :: Entity Schema.User -> ViewUser
  mapFrom (Entity key user) = storageToViewUser key user
