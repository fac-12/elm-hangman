module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Json
import Random
import Regex


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, requestCmd )


type alias Model =
    { apiResponse : List ApiResponse
    , buttons : List Button
    , numOfLives : Int
    , currentWord : String
    , hint : String
    , guess : String
    }


initialModel : Model
initialModel =
    { apiResponse = [ { name = "", capital = "" } ]
    , buttons = defaultButtons
    , numOfLives = 6
    , currentWord = ""
    , hint = ""
    , guess = ""
    }


type alias ApiResponse =
    { name : String
    , capital : String
    }


defaultApi : ApiResponse
defaultApi =
    { name = ""
    , capital = ""
    }


type alias Button =
    { letter : String
    , guess : Bool
    }


defaultButtons : List Button
defaultButtons =
    let
        alphabet =
            [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]
    in
    List.map (\alphabetChar -> { letter = alphabetChar, guess = False }) alphabet


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hangman Game" ]
        , div [] <| renderGuess model <| String.toList model.currentWord
        , div [] (renderbutton model.buttons)
        , button [ onClick FetchWord ] [ text "Reset Game" ]
        ]


renderGuess : Model -> List Char -> List (Html Msg)
renderGuess model guess =
    List.map
        (\letter ->
            p [ class "guessLetter" ]
                [ span [ class <| displayLetterOrNot model letter ] [ text <| toString letter ]
                ]
        )
        guess


displayLetterOrNot : Model -> Char -> String
displayLetterOrNot model letter =
    let
        log1 =
            Debug.log "letter" (String.fromChar letter)

        log =
            Debug.log "regex" (Regex.contains (Regex.regex <| toString letter) model.guess)
    in
    if Regex.contains (Regex.regex <| String.fromChar letter) model.guess then
        "display"
    else
        "displayNone"


renderbutton : List Button -> List (Html Msg)
renderbutton buttons =
    List.map (\everybutton -> button [ onClick <| ButtonClicked everybutton, class <| checkIfClicked everybutton.guess, disabled everybutton.guess ] [ text everybutton.letter ]) buttons


checkIfClicked : Bool -> String
checkIfClicked bool =
    if bool then
        "letter-button button-disabled"
    else
        "letter-button"



-- requestCmd: Cmd Msg


requestCmd =
    Http.send ReceiveStates getRequest


getRequest =
    let
        url =
            "https://locationsng-api.herokuapp.com/api/v1/states"
    in
    Http.get url stateListDecoder


statesDecoder : Json.Decoder ApiResponse
statesDecoder =
    Json.map2 ApiResponse
        (Json.field "name" Json.string)
        (Json.field "capital" Json.string)


stateListDecoder : Json.Decoder (List ApiResponse)
stateListDecoder =
    Json.list statesDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchWord ->
            ( model, requestCmd )

        ReceiveStates (Ok listOfStates) ->
            let
                log =
                    Debug.log "states" listOfStates

                getRandomStateIndex =
                    Random.generate StateIndex (Random.int 0 <| List.length listOfStates)
            in
            ( { model | apiResponse = listOfStates }, getRandomStateIndex )

        ReceiveStates (Err err) ->
            let
                log =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        StateIndex index ->
            let
                log =
                    Debug.log "index" model

                newWord =
                    callMeMaybe (List.head (List.drop index model.apiResponse))

                newGuessLetters =
                    List.map (\x -> ' ') <| String.toList newWord.name
            in
            ( { model | currentWord = String.toUpper newWord.name, hint = newWord.capital }, Cmd.none )

        ButtonClicked clickedButton ->
            let
                updatedButtonList =
                    List.map
                        (\button ->
                            if button.letter == clickedButton.letter then
                                { button | guess = True }
                            else
                                button
                        )
                        model.buttons
            in
            ( { model | guess = model.guess ++ clickedButton.letter, buttons = updatedButtonList }, Cmd.none )


callMeMaybe : Maybe ApiResponse -> ApiResponse
callMeMaybe maybeThing =
    case maybeThing of
        Just singleState ->
            singleState

        Nothing ->
            defaultApi


type Msg
    = FetchWord
    | ReceiveStates (Result Http.Error (List ApiResponse))
    | StateIndex Int
    | ButtonClicked Button
