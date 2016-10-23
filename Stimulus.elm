module Stimulus exposing (..)

{-| Blatantly stolen from the todo-mvc elm example
    https://github.com/evancz/elm-todomvc/blob/master/Todo.elm
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Time exposing (Time, millisecond)
import Time.DateTime as DateTime exposing (zero)


main : Program Never
main =
    App.program
        { init = emptyModel
        , update = update --WithStorage
        , subscriptions = subscriptions
        , view = view
        }

-- port setStorage : Model -> Cmd msg

type alias Model =
    { death : Int
    , entries: Fact
    , currEntry: String
    }

type alias Fact =
  { textt : String
  , frequency : Int
  , numleft: Float
  }

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every (1/ 60) Tick

type Msg
    = NoOp
    | Tick Time

emptyModel : ( Model, Cmd Msg )
emptyModel =
    let dob = DateTime.dateTime { zero | year = 1996, month = 7, day = 11 }
        dobMilli = Time.inMilliseconds (DateTime.toTimestamp dob)
    in { death = (round dobMilli + 2524556160000), entries = { textt = "pizzas left",  frequency = 3, numleft= 0.98349834 }, currEntry = "pizzas left" } ! []

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
        Tick time ->
            let entry = model.entries
                freq = entry.frequency
                death = model.death
            in { model | entries = { entry | numleft = (numLeft death freq time) } }  ! []

numLeft : Int -> Int -> Time -> Float
numLeft death freq time =
    let millisRemaining = (toFloat death) - Time.inMilliseconds time
        millisInMonth = 2629746000
    in 1.0 * (toFloat freq) / millisInMonth * millisRemaining

view : Model -> Html Msg
view {death, entries, currEntry} =
  viewFact entries

viewFact : Fact -> Html Msg
viewFact {textt, frequency, numleft} =
  div
  [ style [ ("color", "blue") ] ]
  [ div [ class "fact-text" ] [ text (textt ++ " " ++ (toString numleft)) ]
  --, div [ class "fact-timesLeft" ] [ text (toString numleft) ]
  ]
