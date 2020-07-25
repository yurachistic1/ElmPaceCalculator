module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (..)
import Time



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL

type TimerStatus 
    = Start
    | Stop
    | Reset

type alias Model =
    { hours : Int
    , mins : Int
    , secs : Int
    , distance : String
    , pace : Int
    , displayResult : Bool
    , error : String
    , timerStatus : TimerStatus
    }


init : () -> (Model, Cmd Msg)
init _ =
    (Model 0 0 0 "" 0 False "" Stop
    , Cmd.none
    )



-- UPDATE


type Msg
    = Hours String
    | Mins String
    | Secs String
    | Distance String
    | CalculatePace
    | Tick Time.Posix
    | TimerStatus TimerStatus


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Hours newHours ->
            ({ model | hours = validateInt newHours model.hours 10000 }
            , Cmd.none
            )
        Mins newMins ->
            ({ model | mins = validateInt newMins model.mins 60 }
            , Cmd.none
            )
        Secs newSecs ->
            ({ model | secs = validateInt newSecs model.secs 60 }
            , Cmd.none
            )
        Distance newDistance ->
            ({ model | distance = validateFl newDistance model.distance }
            , Cmd.none
            )
        CalculatePace ->
            (calculate model
            , Cmd.none
            )
        Tick _ ->
            ( advanceTime model
            , Cmd.none
            )
        TimerStatus status -> 
            ( updateTimerStatus status model
            , Cmd.none
            )


updateTimerStatus : TimerStatus -> Model -> Model
updateTimerStatus status model = 
    case status of
        Start ->
            { model | timerStatus = status }
        Stop -> 
            { model | timerStatus = status }
        Reset -> 
            { model 
            | timerStatus = status
            , hours = 0
            , mins = 0
            , secs = 0
            }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timerStatus of
        Start ->
            Time.every 1000 Tick
        Stop -> 
            Sub.none
        Reset ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ] 
        [ div [ class "row"] [div [ class "col-sm-6 offset-sm-3" ] [ h1 [] [ text "Running Pace Calculator" ] ] ]
        , div [ class "row" ] [ div [ class "col-sm-6 offset-sm-3" ] [ h6 [] [ text "You can enter your time manually or start the timer and stop it when you come back!" ]] ]
        , div [ class "row"]
                [ div [ class "form-group col-sm-6 offset-sm-3"] 
                    [ timerButton model 
                    , button [ id "reset", class "btn btn-warning", onClick (TimerStatus Reset) ] [ text "Reset Timer" ]
                    ]
                ]
        , Html.form []
            [ div [ class "row"]
                [ div [ class "form-group col-sm-2 offset-sm-3" ] [input [ class "form-control", type_ "text", placeholder "Hours", value (intVal model.hours), onInput Hours ] []]
                , div [ class "form-group col-sm-2" ] [input [ class "form-control", type_ "text", placeholder "Mins", value (intVal model.mins), onInput Mins ] []]
                , div [ class "form-group col-sm-2" ] [input [ class "form-control", type_ "text", placeholder "Secs", value (intVal model.secs), onInput Secs ] []]
                ]
            , div [ class "row"] 
                [ div [ class "form-group col-sm-3 offset-sm-3" ] [input [ class "form-control", type_ "text", placeholder "Distance (km)", value model.distance, onInput Distance ] []]
                ]
            ]
        , div [ class "row" ] [ div [ class "col-sm-6 offset-sm-3"] [button [ id "calcbtn", class "btn btn-primary", onClick CalculatePace ] [ text "Calculate" ]]] 
        , viewResult model
        ]



timerButton : Model -> Html Msg
timerButton model = 
    if model.timerStatus == Start then 
        button [ id "timer", class "btn btn-danger", onClick (TimerStatus Stop) ] [ text "Stop Timer" ]
    else 
        button [ id "timer", class "btn btn-success", onClick (TimerStatus Start) ] [ text "Start Timer" ]


viewResult : Model -> Html Msg
viewResult model =
    if model.displayResult then
        div [ class "row" ] [div [ class "col-sm-6 offset-sm-3" ] [ h5 [] [ text (formatPace model.pace)] ] ]

    else
        div [ class "row" ] [div [ class "col-sm-6 offset-sm-3" ] [ span[ class "badge badge-danger" ] [text model.error ] ] ]


formatPace : Int -> String
formatPace pace =
    "Your pace is "
    ++ String.fromInt (pace // 60) 
    ++ ":" 
    ++ String.fromInt (remainderBy 60 pace)
    ++ " per kilometer!"


-- LOGIC


calculate : Model -> Model
calculate model =
    if model.distance /= "0" && model.distance /= "0." then
        case String.toFloat model.distance of
            Just distance ->
                { model | pace = calcPace distance model, displayResult = True}

            Nothing ->
                { model | error = "Please enter a valid distance value", displayResult = False }

    else
        { model | error = "Distance cannot be 0km", displayResult = False }


calcPace : Float -> Model -> Int
calcPace distance model =
    Basics.round ((toFloat model.hours * 3600 + toFloat model.mins * 60 + toFloat model.secs) / distance)


advanceTime : Model -> Model
advanceTime model =
    let
        seconds = model.secs + 1
        minutes = model.mins + seconds // 60
        hours = model.hours + minutes // 60
    in
    
    { model 
    | secs = remainderBy 60 seconds
    , mins = remainderBy 60 minutes
    , hours = hours
    }



-- VALIDATORS


validateInt : String -> Int -> Int -> Int
validateInt input original lim =  
    case String.toInt input of
        Just value ->
            if value < lim then
                value
            else 
                original

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
    case run parserDistance input of
        Ok _ ->
            input

        Err _ ->
            original


parserDistance : Parser (Maybe Float)
parserDistance =
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
