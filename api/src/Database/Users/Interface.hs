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

import Database.Schema (NewUser, User)
import Mapping.ExternalTypes (ViewUser)

type Username = Text

data UserUpdate
  = SendNewEmail Text
  | SendNewPasswordHash Text
  deriving stock (Generic, Show)

data UserStorage :: Effect where
  GetUser :: Username -> UserStorage m ViewUser
  InsertUser :: NewUser -> UserStorage m ViewUser
  EditUser :: Key User -> [UserUpdate] -> UserStorage m ViewUser
  DeleteUser :: Key User -> UserStorage m ()

type instance DispatchOf UserStorage = 'Dynamic

makeEffect ''UserStorage
