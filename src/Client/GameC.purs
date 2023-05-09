module Client.GameC where

import Prelude

import Client.MapMath (angularDirection, haversineDistance)
import Data.Array as Array
import Data.Foldable as Foldable
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Shared.API as API
import Shared.MapData (Point, Properties)
import Shared.Utils as Utils

type State =
  { countries :: Maybe (Map String Properties)
  , target :: Maybe String
  , guess :: String
  , pastGuesses :: List String
  , gameOver :: Boolean
  }

data Action
  = Initialize
  | Restart
  | GiveUp
  | SetGuess String
  | FinalizeGuess

mkInitialState :: forall a. a -> State
mkInitialState _ =
  { countries: Nothing
  , target: Nothing
  , guess: ""
  , pastGuesses: Nil
  , gameOver: false
  }

compareCapitals
  :: forall a. (Point -> Point -> a) -> a -> Map String Properties -> String -> String -> a
compareCapitals f backup countries guess target =
  fromMaybe backup $ do
    { caplat: caplatA, caplong: caplongA } <- Map.lookup guess countries
    { caplat: caplatB, caplong: caplongB } <- Map.lookup target countries
    pure $ f { lat: caplatA, long: caplongA } { lat: caplatB, long: caplongB }

type PastGuess = { guess :: String, distance :: Int, direction :: String }

getPastGuesses :: State -> Array PastGuess
getPastGuesses { pastGuesses, countries: Just countries, target: Just target } =
  Array.reverse $ map guessInfo $ Array.fromFoldable pastGuesses
  where
  guessInfo guess = { guess, distance, direction }
    where
    distance = compareCapitals haversineDistance (-1) countries guess target
    direction = compareCapitals angularDirection "N/A" countries guess target
getPastGuesses _ = []

getPossibleGuesses :: State -> Array String
getPossibleGuesses { countries: Just countries, guess, pastGuesses } =
  Array.filter isValid $ Array.fromFoldable $ Map.keys countries
  where
  isValid name = not (Foldable.elem name pastGuesses) && nameMatches
    where
    nameMatches = String.contains (String.Pattern $ String.toLower guess) $ String.toLower name
getPossibleGuesses _ = []

getGuess :: State -> String
getGuess = _.guess

getTarget :: State -> Maybe Properties
getTarget { countries: Just countries, target: Just target } = Map.lookup target countries
getTarget _ = Nothing

guessCount :: State -> Int
guessCount = _.pastGuesses >>> List.length

guessIsValid :: State -> Boolean
guessIsValid { countries: Just countries, guess } = Map.member guess countries
guessIsValid _ = false

gameOver :: State -> Boolean
gameOver = _.gameOver

gameWon :: State -> Boolean
gameWon { pastGuesses, target: Just target } = Foldable.elem target pastGuesses
gameWon _ = false

checkOver :: State -> State
checkOver state@{ pastGuesses } | gameWon state || List.length pastGuesses >= 7 =
  state { gameOver = true }
checkOver state = state

setGuess :: forall cs o m. MonadAff m => String -> H.HalogenM State Action cs o m Unit
setGuess guess = H.modify_ _ { guess = guess }

finalizeGuess :: forall cs o m. MonadAff m => H.HalogenM State Action cs o m Unit
finalizeGuess = do
  { guess, pastGuesses } <- H.get
  H.modify_ $ checkOver <<< _ { pastGuesses = Cons guess pastGuesses, guess = "" }

restartGame :: forall cs o m. MonadAff m => H.HalogenM State Action cs o m Unit
restartGame = H.get >>= case _ of
  { countries: Just countries } -> do
    target <- H.liftEffect $ Utils.randomElement $ Array.fromFoldable $ Map.keys countries
    --let target = "France"
    H.modify_ _ { target = Just target, guess = "", pastGuesses = Nil, gameOver = false }
  _ -> pure unit

startGame :: forall cs o m. MonadAff m => H.HalogenM State Action cs o m Unit
startGame = do
  countries <- H.liftAff API.getCountries
  let countriesMap = Map.fromFoldable $ map toTuple countries
  H.modify_ _ { countries = Just countriesMap }
  restartGame
  where
  toTuple properties = Tuple properties.cntry_name properties

giveUpGame :: forall cs o m. MonadAff m => H.HalogenM State Action cs o m Unit
giveUpGame = H.modify_ _ { gameOver = true }

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction Initialize = startGame
handleAction Restart = restartGame
handleAction GiveUp = giveUpGame
handleAction (SetGuess guess) = setGuess guess
handleAction FinalizeGuess = finalizeGuess
