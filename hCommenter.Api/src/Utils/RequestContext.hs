{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.RequestContext where

import Optics (makeFieldLabelsNoPrefix)

import Utils.Environment (Env)

data RequestContext = RequestContext
  { env :: !Env
  , correlationId :: !Text
  }

makeFieldLabelsNoPrefix ''RequestContext

mkRequestContext :: Env -> Text -> RequestContext
mkRequestContext = RequestContext
