module Client.Game where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Shared.API as API
import Shared.MapData (Properties)
import Shared.Utils as Utils

type State =
  { countries :: Maybe (Map String Properties)
  , target :: Maybe String
  , count :: Int
  }

data Action
  = Increment
  | Initialize
  | Restart

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { count: 0, countries: Nothing, target: Nothing }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render { target: Just target, count } =
  HH.div_
    [ HH.p_
        [ HH.text $ "You clicked " <> show count <> " times" ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    , HH.p_
        [ HH.text $ "Target is " <> target ]
    ]
render _ = HH.div_ [ HH.text "Loading ..." ]

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction Increment = H.modify_ \st -> st { count = st.count + 1 }
handleAction Restart = H.get >>= case _ of
  { countries: Just countries } -> do
    target <- H.liftEffect $ Utils.randomElement $ Array.fromFoldable $ Map.keys countries
    H.modify_ \st -> st { target = Just target }
  _ -> pure unit
handleAction Initialize = do
  countries <- map Utils.debug_ $ H.liftAff API.getCountries
  let countriesMap = Map.fromFoldable $ map toTuple countries
  H.modify_ \st -> st { countries = Just countriesMap }
  handleAction Restart
  where
  toTuple properties = Tuple properties.cntry_name properties
