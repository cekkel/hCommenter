{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.RequestContext where

import Optics (makeFieldLabelsNoPrefix)

import Utils.Environment (Env)

data RequestContext = RequestContext
  { authUsername :: Maybe Text
  , env :: !Env
  , correlationId :: !Text
  }

makeFieldLabelsNoPrefix ''RequestContext

mkRequestContext :: Env -> Text -> RequestContext
mkRequestContext = RequestContext Nothing
