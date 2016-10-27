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

type Unit = Day | Week | Month | Year

type alias Entry =
    { label : String
    , outro: String
    , rate : Float
    --, unit: Unit  ** Elm can't port non-concrete types, so we have to use a string instead
    , unit: String
    }

type alias ActiveEntry =
    { entry: Entry
    , numleft: Float
    }

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 80 Tick

eightyYearsInMillis =
    1000 * 60 * 60 * 24 * 365 * 80

unitToMillis unit =
    case unit of
        "Day" -> 1000 * 60 * 60 * 24
        "Week" -> 1000 * 60 * 60 * 24 * 7
        "Month" -> 1000 * 60 * 60 * 24 * 30
        "Year" -> 1000 * 60 * 60 * 24 * 365
        _ -> 0

calculateDeath : Date -> Int
calculateDeath dob =
    let dobMilli = Date.toTime dob
    in (round dobMilli + eightyYearsInMillis)

starterEntries =
    [ { label = "full pizzas", outro = "left to eat", rate = 7, unit = "Month" }
    , { label = "phone calls home", outro = "left to make", rate = 2.2, unit = "Month" }
    --, { label = "bobas", outro = "left to drink", rate = 7, unit = "Month" }
    , { label = "movies in theater", outro = "left to watch", rate = 4, unit = "Year" }
    , { label = "visits home", outro = "left to make", rate = 2, unit = "Year" }
    , { label = "sodas", outro = "left to drink", rate = 1, unit = "Week" }
    , { label = "emails", outro = "left to read", rate = 20, unit = "Day" }
    , { label = "sushi dinners", outro = "left to eat", rate = 4, unit = "Year" }
    , { label = "concerts", outro = "left to attend", rate = 1, unit = "Year" }
    , { label = "gym workouts", outro = "left", rate = 2, unit = "Week" }
    , { label = "glasses of water", outro = "left to drink", rate = 3, unit = "Day" }
    , { label = "beach trips", outro = "left to take", rate = 1, unit = "Year" }
    , { label = "hiking trips", outro = "left to take", rate = 2, unit = "Year" }
    ] |> Array.fromList

emptyModel : Model
emptyModel =
    { onboarded = False, dob = "", death = 0, entries = starterEntries, activeEntry = { entry = { label = "", outro = "", rate = 0, unit = "Year" }, numleft = 0.0 } }

type alias Flags = { rand: Int, savedModel: Maybe Model }
init : Flags -> ( Model, Cmd Msg )
init { rand, savedModel } =
    let model = Maybe.withDefault emptyModel savedModel
        index = rand % (Array.length model.entries)
        activeEntry = case Array.get index model.entries of
                        Nothing -> Debug.crash("array index access out of bounds")
                        Just entry -> entry
    in { model | activeEntry = { entry = activeEntry, numleft = 0.0 } } ! []

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
                numleft = numLeft model.death activeEntry.entry time
            in { model | activeEntry = { activeEntry | numleft = numleft } } ! []
        DobChange dob ->
            { model | dob = dob } ! []
        Submit ->
            let dob = case Date.fromString model.dob of
                        Ok date -> date
                        Err msg -> Debug.crash("received a malform string")
                death = calculateDeath dob
            in { model | onboarded = True, death = death } ! []

numLeft : Int -> Entry -> Time -> Float
numLeft death { label, outro, rate, unit } time =
    let millisRemaining = Basics.max 0 ((toFloat death) - Time.inMilliseconds time)
        unitMillis = unitToMillis unit
        unitsLeft = millisRemaining / unitMillis
        numleft = rate * unitsLeft
    in numleft

view : Model -> Html Msg
view model =
    if (not model.onboarded) then
        viewNotOnboarded model
    else
        viewOnboarded model

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
        split = String.split "." (toString numleft)
        front = unsafeHead split "head of numleft string is nothing"
        back = case List.head (List.drop 1 split) of
                Just s -> s
                Nothing -> "0"
        backlength = 9
        paddedback = back |> stringTruncate backlength |> zeroPad backlength
        rate = entry.rate |> toString
        unit = entry.unit |> String.toLower
    in body []
        [ div [ id "app" ]
            [ h2 [ class "count" ] [ text front
                                   , sup [] [ text ".", text paddedback ]
                                   ]
            , h1 [ class "label" ] [ text (String.toUpper entry.label) ]
            , h3 [ class "outro" ] [ text (String.toLower entry.outro) ]
            ]
        , div [ class "footer" ] [ span [] [ text "* " ]
                    , span [] [ text "based on a projected death at age 80" ]
                    , span [] [ text (" and a rate of ~" ++ rate ++ " per " ++ unit) ]
                    ]
        ]

stringTruncate : Int -> String -> String
stringTruncate length str =
    String.dropRight (Basics.max 0 (String.length str - length)) str

zeroPad : Int -> String -> String
zeroPad numZeros str =
    if String.length str < numZeros then
        let paddedStr = str ++ "0"
        in zeroPad numZeros paddedStr
    else
        str

unsafeHead : List a -> String -> a
unsafeHead list error =
    case List.head list of
        Just i -> i
        Nothing -> Debug.crash(error)
