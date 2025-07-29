{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mapping.Users (storageToViewUser) where

import Database.Persist (PersistEntity (Key))
import Optics ((^.))

import Mapping.ExternalTypes (ViewUser (..))

import Database.Schema qualified as Schema

storageToViewUser :: Key Schema.User -> Schema.User -> ViewUser
storageToViewUser _ user =
  ViewUser
    { username = user ^. #userUsername
    , email = user ^. #userEmail
    , createdAt = user ^. #userCreatedAt
    }
