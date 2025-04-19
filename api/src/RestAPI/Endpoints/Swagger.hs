module RestAPI.Endpoints.Swagger where

import Data.Function ((&))
import Data.Swagger
  ( Swagger
  )
import Optics hiding ((&))
import Servant (Get, JSON, (:>))

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

withMetadata :: Swagger -> Swagger
withMetadata swaggerDef =
  swaggerDef
    & #info % #title .~ "hCommenter.API"
    & #info % #version .~ "1.0"
    & #info % #description ?~ "An API for creating and retrieving conversation comments."
    & #info % #license ?~ "MIT"
