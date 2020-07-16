module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { hours : Int
    , mins : Int
    , secs : Int
    , distance : String
    , pace : Int
    , displayResult : Bool
    , error : String
    }


init : Model
init =
    Model 0 0 0 "" 0 False ""



-- UPDATE


type Msg
    = Hours String
    | Mins String
    | Secs String
    | Distance String
    | CalculatePace


update : Msg -> Model -> Model
update msg model =
    case msg of
        Hours newHours ->
            { model | hours = validateInt newHours model.hours }

        Mins newMins ->
            { model | mins = validateInt newMins model.mins }

        Secs newSecs ->
            { model | secs = validateInt newSecs model.secs }

        Distance newDistance ->
            { model | distance = validateFl newDistance model.distance }

        CalculatePace ->
            calculate model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Hours", value (intVal model.hours), onInput Hours ] []
        , text " : "
        , input [ type_ "text", placeholder "Mins", value (intVal model.mins), onInput Mins ] []
        , text " : "
        , input [ type_ "text", placeholder "Secs", value (intVal model.secs), onInput Secs ] []
        , div [] [input [ type_ "text", placeholder "Distance", value model.distance, onInput Distance ] []]
        , div[] [button [ onClick CalculatePace ] [ text "calculate" ]]
        , viewResult model 
        ]


viewResult : Model -> Html Msg
viewResult model =
    if model.displayResult then
        div [] [ text (String.fromInt model.pace) ]

    else
        div [] [ text model.error ]



-- LOGIC


calculate : Model -> Model
calculate model =
    if model.distance /= "0" then
        case String.toFloat model.distance of
            Just distance ->
                { model | pace = calcPace distance model, displayResult = True}

            Nothing ->
                { model | error = "Please enter valid distance value", displayResult = False }

    else
        { model | error = "Distance cannot be 0", displayResult = False }


calcPace : Float -> Model -> Int
calcPace distance model =
    Basics.round ((toFloat model.hours * 3600 + toFloat model.mins * 60 + toFloat model.secs) / distance)



-- VALIDATORS


validateInt : String -> Int -> Int
validateInt input original =
    let
        convIn =
            String.toInt input
    in
    case convIn of
        Just value ->
            value

        Nothing ->
            if input == "" then
                0

            else
                original


intVal : Int -> String
intVal value =
    if value == 0 then
        ""

    else
        String.fromInt value


validateFl : String -> String -> String
validateFl input original =
    case run parser input of
        Ok _ ->
            input

        Err _ ->
            original


parser : Parser (Maybe Float)
parser =
    oneOf
        [ succeed Just
            |= float
            |. end
        , succeed Nothing
            |. int
            |. symbol "."
        , succeed Nothing
            |. end
        ]
