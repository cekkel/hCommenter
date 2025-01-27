module Mapping.Typeclass where

import Database.Persist.Sql (Entity (Entity))
import Database.StorageTypes (Comment)
import Mapping.Comments (storageToViewComment)
import Mapping.ExternalTypes (ViewComment)

class MapsFrom a b where
  mapFrom :: a -> b

instance MapsFrom (Entity Comment) ViewComment where
  mapFrom :: Entity Comment -> ViewComment
  mapFrom (Entity key comment) = storageToViewComment key comment
