{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.AppContext where

import Optics (makeFieldLabelsNoPrefix)

import Utils.Environment (Env)

data AppContext = AppContext
  { env :: !Env
  , correlationId :: !Text
  }

makeFieldLabelsNoPrefix ''AppContext

mkAppContext :: Env -> Text -> AppContext
mkAppContext = AppContext
