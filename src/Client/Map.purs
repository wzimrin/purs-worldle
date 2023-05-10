module Client.Map where

import Prelude

import Client.MapMath as Math
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Graphics.Canvas
  ( Context2D
  , clearRect
  , getCanvasElementById
  , getCanvasHeight
  , getCanvasWidth
  , getContext2D
  , setFillStyle
  )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Shared.API as API
import Shared.MapData (Geometry, Properties)
import Shared.Utils as Utils

type State = { country :: Properties }
data Action
  = Initialize
  | Update Properties

component :: forall q o m. MonadAff m => H.Component q Properties o m
component =
  H.mkComponent
    { initialState: \country -> { country: country }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Update >>> Just
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render _ = HH.canvas [ HP.id "mapCanvas" ]

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction Initialize = do
  { country } <- H.get
  countryData <- H.liftAff $ API.getCountryMap country.gwcode
  drawMap country countryData
handleAction (Update newCountry) = do
  { country } <- H.get
  when (country /= newCountry) $ do
    H.modify_ _ { country = newCountry }
    handleAction Initialize

drawMap
  :: forall cs o m. MonadAff m => Properties -> Geometry -> H.HalogenM State Action cs o m Unit
drawMap country countryData = do
  canvas <- H.liftEffect (getCanvasElementById "mapCanvas" >>= Utils.throwOnNothing "no canvas")
  context <- H.liftEffect $ getContext2D canvas
  width <- H.liftEffect $ getCanvasWidth canvas
  height <- H.liftEffect $ getCanvasHeight canvas
  H.liftEffect $ clearRect context { x: 0.0, y: 0.0, width, height }
  H.liftEffect $ setFillStyle context "black"
  H.liftEffect $ traverse_ (drawShape context) $ Math.mapOf country countryData width height
  pure unit

foreign import drawShape :: Context2D -> Math.Shape -> Effect Unit
