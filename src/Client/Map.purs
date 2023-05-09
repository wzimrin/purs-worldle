module Client.Map where

import Prelude

import Effect (Effect)
import Client.MapMath as Math
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas (Context2D, clearRect, getCanvasElementById, getContext2D, setFillStyle)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Shared.API as API
import Shared.MapData (Properties)
import Shared.Utils as Utils

type State = { country :: Properties}
data Action = Initialize

component :: forall q o m. MonadAff m => H.Component q Properties o m
component =
  H.mkComponent
    { initialState: \properties -> {country: properties}
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render _ = HH.canvas [HP.id "mapCanvas"]

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction Initialize = drawMap

drawMap :: forall cs o m. MonadAff m => H.HalogenM State Action cs o m Unit
drawMap = do
  { country } <- H.get
  countryData <- map Utils.debug_ $ H.liftAff $ API.getCountryMap country.gwcode
  canvas <- H.liftEffect (getCanvasElementById "mapCanvas" >>= Utils.throwOnNothing "no canvas")
  context <- H.liftEffect $ getContext2D canvas
  H.liftEffect $ clearRect context {x: 0.0, y: 0.0, width: 300.0, height: 150.0}
  H.liftEffect $     setFillStyle context "black"
  H.liftEffect $ traverse_ (drawShape context) $ Math.mapOf country countryData 300.0 150.0
  pure unit

foreign import drawShape :: Context2D -> Math.Shape -> Effect Unit
