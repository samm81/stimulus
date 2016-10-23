module Stimulus exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Time exposing (Time, millisecond)
import Time.DateTime as DateTime exposing (zero)
import String
import Array exposing (Array)

main : Program Never
main =
    App.program
        { init = emptyModel
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { death : Int
    , entries: Array Entry
    , activeEntry: ActiveEntry
    }

type alias Entry =
    { label : String
    , perMonth : Int
    }

type alias ActiveEntry =
    { entry: Entry
    , numleft: String
    }

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 80 Tick

calculateDeath : DateTime.DateTime -> Int
calculateDeath dob =
    let dobMilli = Time.inMilliseconds (DateTime.toTimestamp dob)
    in (round dobMilli + 2524556160000)

pizzaEntry =
    { label = "pizzas left",  perMonth = 3 }
pizzaActiveEntry =
    { entry = pizzaEntry,  numleft = "" }
chrisDob =
    DateTime.dateTime { zero | year = 1996, month = 7, day = 11 }
testModel =
    { death = calculateDeath chrisDob, entries = Array.fromList [ pizzaEntry ], activeEntry = pizzaActiveEntry } ! []

emptyModel : ( Model, Cmd Msg )
emptyModel =
    testModel
    --{ death = 0, entries = Array.empty, currentEntry = { entry = {"", 0 }, numleft = "" }} ! []

type Msg
    = NoOp
    | Tick Time

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
        Tick time ->
            let activeEntry = model.activeEntry
                perMonth = activeEntry.entry.perMonth
                death = model.death
                numleft = numLeft death perMonth time
                numleftFormatted = numleft |> toString |> String.dropRight 5
            in { model | activeEntry = { activeEntry | numleft = numleftFormatted } } ! []

numLeft : Int -> Int -> Time -> Float
numLeft death perMonth time =
    let millisRemaining = (toFloat death) - Time.inMilliseconds time
        millisInMonth = 2629746000
    in 1.0 * (toFloat perMonth) / millisInMonth * millisRemaining

stylesheet = 
    let 
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "style.css"
            ]
        children = []
    in
        node tag attrs children

view : Model -> Html Msg
view {death, entries, activeEntry} =
    let factView = viewEntry activeEntry in
    div [] [stylesheet, factView]

viewEntry : ActiveEntry -> Html Msg
viewEntry {entry, numleft} =
    let split = String.split "." numleft
        front = case List.head split of
                    Nothing -> ""
                    Just i -> i
        back = case List.head (List.drop 1 split) of
                    Nothing -> ""
                    Just i -> i
        paddedback = pad0 back
    in div [ id "app" ]
        [ h1 [ class "label" ] [ text (String.toUpper entry.label) ]
        , div [ class "count" ]
            [ text front
            , sup [ class "fact_number-back" ] [ text ".", text paddedback ]
            ]
        ]

pad0 : String -> String
pad0 cur =
    if String.length cur < 8 then
        let newcur = cur ++ "0"
        in pad0 newcur
    else
        cur
