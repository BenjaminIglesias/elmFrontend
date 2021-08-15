module Test exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Html.Attributes exposing (placeholder, href)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

type alias Book =
    { isbn : String
    , title : String
    , author : String
    , pages : String
    , description : String
    }


type alias Model =
    { books : List Book
    , errorMessage : Maybe String
    
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { books = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:5019/books"
        , expect = Http.expectJson DataReceived (list bookDecoder)
        }

type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Book))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, httpCommand )

        DataReceived (Ok books) ->
            ( { model
                | books = books
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
