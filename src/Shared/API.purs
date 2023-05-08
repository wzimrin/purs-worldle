module Shared.API where

import Prelude

import Effect.Aff (Aff)
import Payload.Client (mkClient_, unwrapBody)
import Payload.Client.ClientApi (class ClientApi)
import Payload.Spec (Spec(Spec), GET)
import Shared.MapData (Properties, Geometry)

type APIRoutes =
  ( countries ::
      GET "/countries"
        { params :: {}
        , response :: Array Properties
        }
  , countryMap ::
      GET "/country/<gwcode>"
        { params :: { gwcode :: Int }
        , response :: Geometry
        }
  )

type APISpec = Spec { | APIRoutes }

apiSpec :: APISpec
apiSpec = Spec

apiClient :: forall a. ClientApi { | APIRoutes } a => a
apiClient = mkClient_ apiSpec

getCountries :: Aff (Array Properties)
getCountries = unwrapBody $ apiClient.countries {}

getCountryMap :: Int -> Aff Geometry
getCountryMap gwcode = unwrapBody $ apiClient.countryMap { params: { gwcode } }
