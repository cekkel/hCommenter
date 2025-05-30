{-# LANGUAGE QuasiQuotes #-}

module Middleware.ServantErrorFormatters (customFormatters) where

import Data.Aeson (object, (.=))
import Network.Wai (Request (rawPathInfo))
import PyF (fmt)
import Servant
  ( ErrorFormatter
  , ErrorFormatters (bodyParserErrorFormatter, notFoundErrorFormatter)
  , JSON
  , NotFoundErrorFormatter
  , ServerError (errBody, errHeaders)
  , defaultErrorFormatters
  , err400
  , err404
  , getAcceptHeader
  )
import Servant.API.ContentTypes (handleAcceptH)

customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = customFormatter
    , notFoundErrorFormatter = notFoundFormatter
    }

customFormatter :: ErrorFormatter
customFormatter tr req err =
  let
    -- aeson Value which will be sent to the client
    value = object ["combinator" .= tshow tr, "error" .= err]
    -- Accept header of the request
    accH = getAcceptHeader req
  in
    -- handleAcceptH is Servant's function that checks whether the client can accept a
    -- certain message type.
    -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
    case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
      -- If client can't handle JSON, we just return the body the old way
      Nothing -> err400 {errBody = fromString err}
      -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
      Just (ctypeH, body) ->
        err400
          { errBody = body
          , errHeaders = [("Content-Type", toStrict ctypeH)]
          }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404 {errBody = [fmt|Not found path: {rawPathInfo req}|]}
