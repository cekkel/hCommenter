{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Users.Interface
  ( getUser
  , insertUser
  , editUser
  , deleteUser
  , UserStorage (..)
  , UserUpdate (..)
  )
where

import Database.Persist.Sql (Key)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect)
import Effectful.TH (makeEffect)

import Mapping.ExternalTypes (ViewUser)

import Database.Schema qualified as Schema

type Username = Text

data UserUpdate
  = SendNewEmail Text
  | SendNewPasswordHash Text
  deriving stock (Generic, Show)

data UserStorage :: Effect where
  GetUser :: Username -> UserStorage m ViewUser
  -- TODO: Fix this, move NewUser out of Schema
  InsertUser :: Schema.NewUser -> UserStorage m (Key Schema.User)
  EditUser :: Key Schema.User -> [UserUpdate] -> UserStorage m ViewUser
  DeleteUser :: Key Schema.User -> UserStorage m ()

type instance DispatchOf UserStorage = 'Dynamic

makeEffect ''UserStorage
