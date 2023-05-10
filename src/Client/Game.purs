module Client.Game where

import Prelude

import Client.GameC
  ( Action(..)
  , PastGuess
  , State
  , gameOver
  , gameWon
  , getGuess
  , getPastGuesses
  , getPossibleGuesses
  , getTarget
  , guessCount
  , guessIsValid
  , handleAction
  , mkInitialState
  )
import Client.HtmlUtils (classes)
import Client.Map as CMap
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Shared.MapData (Properties)
import Shared.Utils as Utils
import Type.Proxy (Proxy(..))

type Slots = (map :: forall query. H.Slot query Void Int)

_map = Proxy :: Proxy "map"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: mkInitialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

renderPastGuess :: forall w i. PastGuess -> Array (HH.HTML w i)
renderPastGuess { guess, distance, direction } =
  [ HH.div [ HP.title guess ] [ HH.text guess ]
  , HH.div_ [ HH.text $ show distance <> " km" ]
  , HH.div_ [ HH.text $ direction ]
  ]

renderPastGuesses :: forall w i. Array PastGuess -> Array (HH.HTML w i)
renderPastGuesses guesses = guessDivs <> extraDivs
  where
  guessDivs = guesses >>= renderPastGuess
  extraDivs = Array.replicate ((7 - Array.length guesses) * 3) $ HH.div_ []

renderPossibleGuess :: forall w. String -> HH.HTML w Action
renderPossibleGuess guess = HH.div [ HE.onClick \_ -> SetGuess guess ] [ HH.text guess ]

guesser :: forall w. State -> HH.HTML w Action
guesser state =
  HH.div_
    [ HH.div_
        [ HH.input [ HP.type_ HP.InputText, HP.value $ getGuess state, HE.onValueInput SetGuess ]
        , HH.button
            [ HE.onClick \_ -> FinalizeGuess, HP.disabled $ not $ guessIsValid state ]
            [ HH.text "Guess" ]
        ]
    , HH.div [ classes [ "possibleGuesses" ] ] $ map renderPossibleGuess $ getPossibleGuesses state
    ]

gameFinished :: forall w. State -> Properties -> HH.HTML w Action
gameFinished state target =
  HH.div [ classes [ "gameFinished" ] ]
    [ HH.p_
        [ HH.text $ "You " <> (if gameWon state then "won" else "lost") <> " in "
            <> show (guessCount state)
            <> " guesses!"
        ]
    , HH.p_ [ HH.text $ "The answer was " <> target.cntry_name ]
    ]

gameStateButton :: forall w. State -> HH.HTML w Action
gameStateButton state = HH.button [ HE.onClick \_ -> action ] [ HH.text text ]
  where
  { action, text } =
    if gameOver state then { action: Restart, text: "Restart" }
    else { action: GiveUp, text: "Give Up" }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = HH.div [ classes [ "container" ] ]
  [ case getTarget state of
      Nothing -> HH.div_ [ HH.text "Loading ..." ]
      Just target -> HH.div [ classes [ "game" ] ]
        [ HH.div [ classes [ "map" ] ] [ HH.slot_ _map 0 CMap.component target ]
        , HH.div [ classes [ "gameMain" ] ]
            [ HH.div [ classes [ "playArea" ] ]
                [ if gameOver state then gameFinished state target else guesser state ]
            , HH.div [ classes [ "pastGuesses" ] ] $ renderPastGuesses $ Utils.debug_ $
                getPastGuesses state
            ]
        , HH.div [ classes [ "gameStateButton" ] ] [ gameStateButton state ]
        ]
  ]
