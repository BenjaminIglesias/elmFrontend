module Greeter exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

type alias Greeter =
    { name: String
    , greeting: String
    , age: Int
    , address: Address
    }

type alias Address =
    { street: String
    , city: String
    }

addressDecoder: Decode.Decoder Address
addressDecoder =
    Decode.map2 Address
        (Decode.field "street" Decode.string)
        (Decode.field "city" Decode.string)

greeterDecoder : Decode.Decoder Greeter
greeterDecoder =
    Decode.map4 Greeter
        (Decode.field "name" Decode.string)
        (Decode.field "greeting" Decode.string)
        (Decode.field "age" Decode.int)
        (Decode.field "address" addressDecoder)

encodeAddress : Address -> Encode.Value
encodeAddress address =
    Encode.object
        [ ("street", Encode.string address.street)
        , ("city", Encode.string address.city)
        ]

encodeGreeter : Greeter -> Encode.Value
encodeGreeter greeter =
    Encode.object
        [ ("name", Encode.string greeter.name)
        , ("greeting", Encode.string greeter.greeting)
        , ("age", Encode.int greeter.age)
        , ("address", encodeAddress greeter.address)
        ]