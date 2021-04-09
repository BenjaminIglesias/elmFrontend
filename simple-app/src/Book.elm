module Book exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

type alias Book =
     { isbn : String
    , title : String
    , author : String
    , pages : String
    , description : String
    }

type alias Book2 =
     { isbn : String
    , title : String
    , author : String
    , pages : String
    , description : String
    , addingResult: String
    }

bookDecoder : Decode.Decoder Book
bookDecoder =
    Decode.map5 Book
        (Decode.field "isbn" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "author" Decode.string)
        (Decode.field "pages" Decode.string)
        (Decode.field "description" Decode.string)

encodeBook : Book2 -> Encode.Value
encodeBook book =
    Encode.object
        [ ("isbn", Encode.string book.isbn)
        , ("title", Encode.string book.title)
        , ("author", Encode.string book.author)
        , ("pages", Encode.string book.pages)
        , ("description", Encode.string book.description)
        ]
