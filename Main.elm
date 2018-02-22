module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Json


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
    { apiResponse = { secretword = "", hint = "" }
    , buttons = []
    , guessLetters = []
    , numOfLives = 6
    }


type alias Model =
    { apiResponse : ApiResponse
    , buttons : List Button
    , guessLetters : List BlankSpace
    , numOfLives : Int
    }


type alias ApiResponse =
    { secretword : String
    , hint : String
    }


defaultApi : ApiResponse
defaultApi =
    { secretword = ""
    , hint = ""
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
        ]


renderbutton : List Button -> List (Html Msg)
renderbutton buttons =
    List.map (\everybutton -> button [] [ text everybutton.letter ]) buttons


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


type Msg
    = FetchWord
    | GuessLetter
