module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Json
import Random



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
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { apiResponse = [ { name = "", capital = "" } ]
    , buttons = []
    , guessLetters = []
    , numOfLives = 6
    , currentWord = ""
    }


type alias Model =
    { apiResponse : List ApiResponse
    , buttons : List Button
    , guessLetters : List BlankSpace
    , numOfLives : Int
    , currentWord : String
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


type alias BlankSpace =
    { letter : String
    , display : Bool
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hangman Game" ]
        , div [] (renderbutton defaultButtons)
        , button [ onClick FetchWord ] [ text "press me!" ]
        ]


renderbutton : List Button -> List (Html Msg)
renderbutton buttons =
    List.map (\everybutton -> button [] [ text everybutton.letter ]) buttons



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

        GuessLetter ->
            ( model, Cmd.none )

        ReceiveStates (Ok listOfStates) ->
            let 
                log = 
                    Debug.log "states" listOfStates
            in
              
              ( { model | apiResponse = listOfStates } , Random.generate StateIndex (Random.int 0 (List.length listOfStates)) )

        ReceiveStates (Err err) ->
            let
                log =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        StateIndex index  ->

            let log = 
                Debug.log "index" model
                newWord = callMeMaybe (List.head (List.drop index model.apiResponse))
            in

            ( { model | currentWord = newWord.name } , Cmd.none )


callMeMaybe : Maybe ApiResponse -> ApiResponse
callMeMaybe maybeThing =
    case maybeThing of 
        Just singleState -> singleState
        Nothing -> defaultApi


type Msg
    = FetchWord
    | GuessLetter
    | ReceiveStates (Result Http.Error (List ApiResponse))
    | StateIndex Int 
