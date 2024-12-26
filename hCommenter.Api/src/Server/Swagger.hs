module Server.Swagger where

import ClassyPrelude
import Control.Lens ((&), (.~), (?~))
import Data.Swagger
  ( HasDescription (description)
  , HasInfo (info)
  , HasLicense (license)
  , HasTitle (title)
  , HasVersion (version)
  , Swagger
  )
import Servant (Get, JSON, (:>))

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

withMetadata :: Swagger -> Swagger
withMetadata swaggerDef =
  swaggerDef
    & info . title .~ "hCommenter.API"
    & info . version .~ "1.0"
    & info . description ?~ "An API for creating and retrieving conversation comments."
    & info . license ?~ "MIT"
