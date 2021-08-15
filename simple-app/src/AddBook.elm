module AddBook exposing (..)


import Browser
import Book exposing (Book, encodeBook)
import ErrorHandler
import Html exposing ( Html, a, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http




-- MAIN


main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : Book -> Sub Msg
subscriptions _ = Sub.none







type AddingBookModel
    = AddingBook Book

type alias Book =
     { isbn : String
    , title : String
    , author : String
    , pages : String
    , description : String           
    , addingResult : String
    }


init : () -> (Book, Cmd Msg)
init _ =
    (Book "" "" "" "" "" "" , Cmd.none)

-- UPDATE

type Msg
  = Isbn String
  | Title String
  | Author String
  | Pages String
  | Description String
  | AddBook Book
  | AddBookResult (Result Http.Error String)

update : Msg -> Book -> (Book, Cmd Msg)
update msg book =
  case msg of
    Isbn isbn ->
      ({ book | isbn = isbn }, Cmd.none)

    Title title ->
      ({ book | title = title }, Cmd.none)

    Author author ->
      ({ book | author = author }, Cmd.none)

    Pages pages ->
      ({ book | pages = pages }, Cmd.none)

    Description description ->
       ({ book | description = description }, Cmd.none)
 
    AddBook b ->
       (book, addBook b)
    AddBookResult result ->
        case result of
            Ok message -> ({ book | addingResult = message }, Cmd.none)
            Err error -> ({ book | isbn = ""
                                       , title = ""
                                       , author = ""
                                       , pages = ""
                                       , description = ""
                                       , addingResult = ErrorHandler.toString error }, Cmd.none)


addBook : Book -> Cmd Msg
addBook book = Http.post
    { url = "http://localhost:4711/saveBook"
    , body = Http.jsonBody (encodeBook book)
    , expect = Http.expectString AddBookResult
    }


-- VIEW


view : Book -> Html Msg
view book =
  div []
    [ viewInput "text" "Isbn" book.isbn Isbn
    , viewInput "text" "Title" book.title Title
    , viewInput "text" "Author" book.author Author
    , viewInput "text" "Pages" book.pages  Pages
    , viewInput "text" "Description" book.description Description
    , button [  style "background-color" "#4CAF50", style "border-radius" "8px",onClick (AddBook book)] [ text "Add Book" ]
    , p [] [ text ("Server-response: "++book.addingResult) ]
    ,  button[ style "background-color" "#4CAF50", style "border-radius" "8px"] [a [href "http://localhost:8000/src/DecodingJson.elm"] [ text "Click Here to see all books"]]

    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
