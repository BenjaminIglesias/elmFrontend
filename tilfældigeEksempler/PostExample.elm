-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--
module PostExample exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Encode as Encode
import Json.Decode as Decode



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL
type alias Model =
    { isbn : String
    , title : String
    , author : String
    , pages : Int
    , description : String
   }



init : Model
init =
  Model "" "" "" -1 "" 



-- UPDATE


type Msg
  = Isbn String
  | Title String
  | Author String
  | Description String
  | GotText (Result Http.Error String)



update : Msg -> Model -> Model
update msg model =
  case msg of
    Isbn isbn ->
      { model | isbn = isbn }

    Title title ->
      { model | title = title }

    Author author ->
      { model | author = author }
    
    Description description ->
      { model | description = description }
    
    GotText gotText ->
      { model | gotText = gotText }  
    
    


encodeBook : Model -> Encode.Value
encodeBook book =
    Encode.object
        [ ("isbn", Encode.string book.isbn)
        , ("title", Encode.string book.title)
        , ("author", Encode.string book.author)
        , ("pages", Encode.int book.pages)
        , ("description", Encode.string book.description)
        ]
bookDecoder : Decode.Decoder Model
bookDecoder =
    Decode.map5 Model
        (Decode.field "isbn" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "pages" Decode.int)
        (Decode.field "description" Decode.string)

saveBook : Model -> Cmd Msg
saveBook book = Http.post
    { url = "http://localhost:4711/saveGreeter"
    , body = Http.jsonBody (encodeBook book)
    , expect = Http.expectString GotText

    }



-- VIEW


view : Model -> Html Msg
view model =
 div[][
  div []
    [ 
    viewInput "text" "ISBN" model.isbn Isbn
    , viewInput "text" "Title" model.title Title
    , viewInput "text" "Author" model.author Author
    , viewInput "text" "Description" model.description Description
      ], div [] [ text ( "{ISBN: " ++ model.isbn ++ "} {Title: " ++ model.title ++"}" ++ "{Author: " ++ model.author ++ "}" ++ "{Description: " ++ model.description ++ "}" )  ]
    ,   button [ ] [ text "Send Book To DB"  ]  
      ]



viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
