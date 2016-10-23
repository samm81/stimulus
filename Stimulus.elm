port module Stimulus exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick, onInput)
import Date exposing (Date)
import Time exposing (Time, millisecond)
import String
import Array exposing (Array)

main =
    App.programWithFlags
        { init = init
        , update = updateWithStorage
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { onboarded: Bool
    , dob: String
    , death : Int
    , entries: Array Entry
    , activeEntry: ActiveEntry
    }

type alias Entry =
    { label : String
    , perMonth : Float
    }

type alias ActiveEntry =
    { entry: Entry
    , numleft: String
    }

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 80 Tick

calculateDeath : Date -> Int
calculateDeath dob =
    let dobMilli = Date.toTime dob
    in (round dobMilli + 2524556160000)

starterEntries =
    [ { label = "pizzas", perMonth = 0.6 }
    , { label = "phone calls home", perMonth = 2.2}
    , { label = "bobas", perMonth = 7 }
    , { label = "movies", perMonth = 0.3 }
    , { label = "visits home", perMonth = 0.2 }
    , { label = "sodas", perMonth = 4.0 }
    , { label = "emails", perMonth = 20.0 * 30.0 }
    , { label = "sushi dinners", perMonth = 0.33 }
    , { label = "concerts", perMonth = 0.08 }
    ] |> Array.fromList

emptyModel : Model
emptyModel =
    { onboarded = False, dob = "", death = 0, entries = starterEntries, activeEntry = { entry = { label = "", perMonth = 0.0 }, numleft = "" } }

type alias Flags = { rand: Int, savedModel: Maybe Model }
init : Flags -> ( Model, Cmd Msg )
init { rand, savedModel } =
    let model = Maybe.withDefault emptyModel savedModel
        index = rand % (Array.length model.entries)
        activeEntry = case Array.get index model.entries of
                        Nothing -> Debug.crash("array index access out of bounds")
                        Just entry -> entry
    in { model | activeEntry = { entry = activeEntry, numleft = "" } } ! []

type Msg
    = NoOp
    | Tick Time
    | DobChange String
    | Submit

port setStorage : Model -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )

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
        DobChange dob ->
            { model | dob = dob } ! []
        Submit ->
            let dob = case Date.fromString model.dob of
                        Ok date -> date
                        Err msg -> Debug.crash("received a malform string")
                death = calculateDeath dob
            in { model | onboarded = True, death = death } ! []

numLeft : Int -> Float -> Time -> Float
numLeft death perMonth time =
    let millisRemaining = (toFloat death) - Time.inMilliseconds time
        millisInMonth = 2629746000
    in 1.0 * perMonth / millisInMonth * millisRemaining

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
view model =
    let body = 
        if (not model.onboarded) then
            viewNotOnboarded model
        else
            viewOnboarded model
    in div [] [stylesheet, body]

viewNotOnboarded : Model -> Html Msg
viewNotOnboarded model =
    div []
        [ h1 [ id "dob", class "label" ] [ text "When were you born?" ]
        , footer []
            [ input [type' "date", name "dob", id "dob", onInput DobChange] []
            , button [onClick Submit] [text "Motivate"]
            ]
        ]


viewOnboarded : Model -> Html Msg
viewOnboarded { onboarded, death, entries, activeEntry } =
    let { entry, numleft } = activeEntry
        split = String.split "." numleft
        front = case List.head split of
                    Nothing -> ""
                    Just i -> i
        back = case List.head (List.drop 1 split) of
                    Nothing -> ""
                    Just i -> i
        paddedback = zeroPad 9 back
    in div [ id "app" ]
        [ h2 [ class "count" ]
            [ text front
            , sup [] [ text ".", text paddedback ]
            ]
        , h1 [ class "label" ] [ text (String.toUpper entry.label) ]
        ]

zeroPad : Int -> String -> String
zeroPad numZeros str =
    if String.length str < numZeros then
        let paddedStr = str ++ "0"
        in zeroPad numZeros paddedStr
    else
        str

pizzaEntry =
    { label = "pizzas left",  perMonth = 1.0 }
pizzaActiveEntry =
    { entry = pizzaEntry,  numleft = "" }
chrisDob =
    case Date.fromString "07-01-1996" of
        Ok date -> date
        Err msg -> Debug.crash("no :(")
testModel =
    { onboarded = True, dob = "02-04-1996", death = calculateDeath chrisDob, entries = Array.fromList [ pizzaEntry ], activeEntry = pizzaActiveEntry } ! []
