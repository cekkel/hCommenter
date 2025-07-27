{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mapping.Users () where

import Database.Persist (Entity (..))
import Optics ((^.))

import Database.Schema (User)
import Mapping.ExternalTypes (ViewUser (..))
import Mapping.Typeclass (MapsFrom (..))

instance MapsFrom (Entity User) ViewUser where
  mapFrom :: Entity User -> ViewUser
  mapFrom (Entity _key user) =
    ViewUser
      { username = user ^. #username
      , email = user ^. #email
      , createdAt = user ^. #createdAt
      }
