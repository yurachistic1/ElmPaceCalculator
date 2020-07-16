module Main exposing (..)

import Browser
import Parser exposing (..)
import Html exposing (Html, div, text, button, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = 
    { hours : Int
    , mins : Int
    , secs : Int
    , distance : String
    , pace : Float
    , speed : Float
    , displayResult : Bool
    }


init : Model
init =
    Model 0 0 0 "" 0 0 False


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
            { model | hours = validateInt newHours model.hours}
        Mins newMins ->
            { model | mins = validateInt newMins model.mins}
        Secs newSecs ->
            { model | secs = validateInt newSecs model.secs}
        Distance newDistance ->
            { model | distance = validateFl newDistance model.distance }
        CalculatePace ->
            { model | pace = 1, speed = 1}


-- VIEW


view : Model -> Html Msg
view model =
    div []
    [ input [ type_ "text", placeholder "Hours", value (intVal model.hours), onInput Hours ] []
    , input [ type_ "text", placeholder "Mins", value (intVal model.mins), onInput Mins ] []
    , input [ type_ "text", placeholder "Secs", value (intVal model.secs), onInput Secs ] []
    , input [ type_ "text", placeholder "Distance", value model.distance, onInput Distance] []
    , button [ onClick CalculatePace ] [ text "calculate" ]
    , div [] [text ((String.fromFloat model.pace) ++ " " ++ (String.fromFloat model.speed))]
    ]


viewResult : Model -> Html Msg
viewResult model =
    let
        isPace : Bool
        isPace = model.pace == 0
        
        isSpeed : Bool
        isSpeed = model.speed == 0

        distanceEmpty : Bool
        distanceEmpty = String.isEmpty model.distance

        distanceDotEnd : Bool
        distanceDotEnd = String.endsWith "." model.distance
    in
    
    if model.pace == 0 && model.speed == 0 then
        div [] []
    else 
        div [] [] 


-- VALIDATORS

validateInt : String -> Int -> Int
validateInt input original =
    let
        convIn = String.toInt input
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