port module Stimulus exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick, onInput, on)
import Date exposing (Date)
import Time exposing (Time, millisecond)
import String
import Array exposing (Array)

import Json.Decode
import Mouse exposing (Position)
import Round exposing (round)

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
    , drag: Maybe Drag
    }

type alias Drag =
    { start: Position
    , current: Position
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
    in (Basics.round dobMilli + eightyYearsInMillis)

starterEntries =
    [ ( Entry "full pizzas" "left to eat" 7 "Month" )
    , ( Entry "phone calls home" "left to make" 2.2 "Month" )
    , ( Entry "bobas" "left to drink" 7 "Month" )
    , ( Entry "movies in theater" "left to watch" 4 "Year" )
    , ( Entry "visits home" "left to make" 2 "Year" )
    , ( Entry "sodas" "left to drink" 1 "Week" )
    , ( Entry "emails" "left to read" 20 "Day" )
    , ( Entry "sushi dinners" "left to eat" 4 "Year" )
    , ( Entry "concerts" "left to attend" 1 "Year" )
    , ( Entry "gym workouts" "left" 2 "Week" )
    , ( Entry "glasses of water" "left to drink" 3 "Day" )
    , ( Entry "beach trips" "left to take" 1 "Year" )
    , ( Entry "hiking trips" "left to take" 2 "Year" )
    , ( Entry "hours to sleep" "" 7 "Day" )
    , ( Entry "elections" "to vote in" 0.25 "Year" )
    , ( Entry "books" "left to read" 10 "Year" )
    , ( Entry "meals" "left to cook" 5 "Week" )
    , ( Entry "videogames" "left to complete" 2 "Year" )
    , ( Entry "board games" "left to play" 5 "Month" )
    , ( Entry "times to check facebook" "" 2 "Day" )
    , ( Entry "museums" "left to visit" 2 "Year" )
    , ( Entry "thanksgivings" "left to have" 1 "Year" )
    , ( Entry "christmases" "left to have" 1 "Year" )
    , ( Entry "halloweens" "left to have" 1 "Year" )
    , ( Entry "trips abroad" "left to take" 1 "Year" )
    ] |> Array.fromList

noMoreEntriesEntry =
    ( Entry "you have no non-zero entries!" "" -0.000000000001 "Year" ) -- the tiny negative number is to trick the warning into not appearing

emptyModel : Model
emptyModel =
    (Model False "" 0 starterEntries (ActiveEntry (Entry "" "" 0 "Year") 0.0) Nothing)

type alias Flags = { rand: Int, time: Time, savedModel: Maybe Model }
init : Flags -> ( Model, Cmd Msg )
init { rand, time, savedModel } =
    let model = Maybe.withDefault emptyModel savedModel
        validEntries = Array.filter (\entry -> entry.rate /= 0) model.entries
        numValidEntries = (Array.length validEntries)
        activeEntry =
            case numValidEntries of
                0 -> noMoreEntriesEntry
                _ -> case Array.get (rand % numValidEntries) validEntries of
                        Nothing -> Debug.crash("array index access out of bounds")
                        Just entry -> entry
        numleft = numLeft model.death activeEntry time
    in { model | activeEntry = { entry = activeEntry, numleft = numleft } } ! []

-- UPDATE

type Msg
    = Tick Time
    | DobChange String
    | Submit
    | DragStart Position
    | DragAt Position
    | DragEnd Position

port setStorage : Model -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel , Cmd.batch [ setStorage newModel, cmds ] )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )

updateHelp : Msg -> Model -> Model
updateHelp msg model =
    case msg of
        Tick time ->
            let activeEntry = model.activeEntry
                numleft = numLeft model.death activeEntry.entry time
            in { model | activeEntry = { activeEntry | numleft = numleft } }
        DobChange dob ->
            { model | dob = dob }
        Submit ->
            let dob = case Date.fromString model.dob of
                        Ok date -> date
                        Err msg -> Debug.crash("received a malform string")
                death = calculateDeath dob
            in { model | onboarded = True, death = death }
        DragStart pos ->
            { model | drag = (Just (Drag pos pos)) }
        DragAt pos ->
            let activeEntry = model.activeEntry
                entry = activeEntry.entry
                modifier = 0.05
                adjRateAt = adjustedRate modifier
                lastPos = (Maybe.withDefault (Drag pos pos) model.drag).current
                adjRate = adjRateAt entry.rate lastPos pos
                newActiveEntry = { activeEntry | entry = { entry | rate = adjRate } }
                newDrag = (Maybe.map (\{start} -> Drag start pos) model.drag)
            in { model | activeEntry = newActiveEntry, drag = newDrag }
        DragEnd _ ->
            let activeEntryName = model.activeEntry.entry.label
                activeEntryRate = model.activeEntry.entry.rate
                updateEntry entry =
                    if entry.label == activeEntryName then
                       { entry | rate = activeEntryRate }
                    else
                        entry
            in { model | drag = Nothing, entries = Array.map updateEntry model.entries }

numLeft : Int -> Entry -> Time -> Float
numLeft death { label, outro, rate, unit } time =
    let millisRemaining = Basics.max 0 ((toFloat death) - Time.inMilliseconds time)
        unitMillis = unitToMillis unit
        unitsLeft = millisRemaining / unitMillis
        numleft = rate * unitsLeft
    in numleft

adjustedRate: Float -> Float -> Position -> Position -> Float
adjustedRate adjustPerPixel currentRate lastPos currentPos =
    let dist = currentPos.x - lastPos.x
        delta = (toFloat dist) * adjustPerPixel
        newRate = Basics.max (currentRate + delta) 0
        newRateRounded = Round.roundNum 1 newRate
    in newRateRounded

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ (tickSubscription model), (mouseSubscription model) ]

tickSubscription model =
    Time.every 80 Tick

mouseSubscription model =
    case model.drag of
        Nothing ->
            Sub.none
        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

-- VIEW

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
viewOnboarded ({ onboarded, death, entries, activeEntry } as model) =
    let { entry, numleft } = activeEntry
        numleftRounded = (Round.round 9 numleft)
        split = String.split "." numleftRounded
        front = unsafeHead split "head of numleft string is nothing"
        back = case List.head (List.drop 1 split) of
                Just s -> s
                Nothing -> "0"
        rate = entry.rate |> Round.round 1
        unit = entry.unit |> String.toLower
        warningClass = if entry.rate == 0 then "warning" else "invisible"
    in body []
        [ div [ id "app" ]
            [ h4 [ class warningClass ] [ text "THIS ENTRY WILL NO LONGER APPEAR" ]
            , h2 [ class "count" ] [ text front
                                   , sup [] [ text ".", text back ]
                                   ]
            , h1 [ class "label" ] [ text (String.toUpper entry.label) ]
            , h3 [ class "outro" ] [ text (String.toLower entry.outro) ]
            ]
        , div [ class "footer" ]
            [ span [] [ text "* " ]
            , span [] [ text "based on a projected death at age 80" ]
            , span [] [ text " and a rate of " ]
            , span [ onMouseDown, class "rate" ] [ text ("~" ++ rate) ]
            , span [] [ text " per " ]
            , span [ class "unit" ] [ text unit ]
            ]
        ]

onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.Decode.map DragStart Mouse.position)

unsafeHead : List a -> String -> a
unsafeHead list error =
    case List.head list of
        Just i -> i
        Nothing -> Debug.crash(error)
