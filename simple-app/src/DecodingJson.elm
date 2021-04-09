module DecodingJson exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Html.Attributes exposing (placeholder, href)


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


view : Model -> Html Msg
view model =
    div []
        [    input [ placeholder "name" ] []
           , button [ onClick SendHttpRequest ]
            [ text "Retrieve Books" ]
            , p[] [a [href "http://localhost:8000/src/AddBook.elm"] [ text "Click Here to add a new Book"]]
        , viewBooksOrError model
        ]


viewBooksOrError : Model -> Html Msg
viewBooksOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewBooks model.books


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewBooks : List Book -> Html Msg
viewBooks books =
    div []
        [ h3 [] [ text "Books:" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewBook books)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ISBN" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Author" ]
        , th []
            [ text "Pages" ]
        , th []
            [ text "Description" ]
                    
                
        ]


viewBook : Book -> Html Msg
viewBook book =
    tr []
        [ td []
            [ text book.isbn ]
        , td []
            [ text book.title ]
        , td []
            [ text book.author ]
        , td []
            [ text book.pages ]
        , td []
            [ text book.description ]
      
        ]


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Book))


bookDecoder : Decoder Book
bookDecoder =
    map5 Book
        (field "isbn" string)
        (field "title" string)
        (field "author" string)
        (field "pages" string)
        (field "description" string)


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:4711/book"
        , expect = Http.expectJson DataReceived (list bookDecoder)
        }


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { books = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }