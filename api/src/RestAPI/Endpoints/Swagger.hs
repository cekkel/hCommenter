module RestAPI.Endpoints.Swagger where

import Data.OpenApi (OpenApi)
import Optics hiding ((&))
import Servant (Get, JSON, (:>))

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

withMetadata :: OpenApi -> OpenApi
withMetadata swaggerDef =
  swaggerDef
    & #info
    % #title
    .~ "hCommenter.API"
    & #info
    % #version
    .~ "1.0"
    & #info
    % #description
    ?~ "An API for creating and retrieving conversation comments."
    & #info
    % #license
    ?~ "MIT"
