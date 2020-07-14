module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = 
    { time : String
    , distance : String
    , pace : Float
    , speed : Float
    }


init : Model
init =
    Model "" "" 0 0


-- UPDATE

type Msg
    = Time String
    | Distance String
    | CalculatePace 


update : Msg -> Model -> Model
update msg model =
    case msg of
        Time newTime ->
            { model | time = newTime }
        Distance newDistance ->
            { model | distance = newDistance }
        CalculatePace ->
            { model | pace = 1, speed = 1}


-- VIEW


view : Model -> Html Msg
view model =
    div []
    [ input [ type_ "text", placeholder "Time", onInput Time ] []
    , input [ type_ "text", placeholder "Distance", onInput Distance] []
    , button [ onClick CalculatePace ] [ text "calculate" ]
    , div [] [text (String.fromFloat model.pace)]
    , div [] [text (String.fromFloat model.speed)]
    ]