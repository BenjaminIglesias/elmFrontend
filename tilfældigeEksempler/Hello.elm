module Hello exposing (..)

import Browser

import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Greeter exposing (Greeter, greeterDecoder, encodeGreeter)

-- main : Program flags ...
main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type Model
  = Failure String
  | Saving String
  | Waiting
  | Loading
  | Succes Greeter

type Message
  = TryAgainPlease
  | GreetingResult (Result Http.Error Greeter)
  | SaveStuff Greeter
  | SaveResult (Result Http.Error String)

init : () -> (Model, Cmd Message)
init _ = (Waiting, Cmd.none)

handleError : Http.Error -> (Model, Cmd Message)
handleError error =
    case error of
        Http.NetworkError ->
          (Failure "Network Error", Cmd.none)
        Http.BadBody err ->
          (Failure <| "Bad Body: "++err, Cmd.none)
        Http.Timeout ->
          (Failure "Timeout", Cmd.none)
        Http.BadStatus status ->
          (Failure <| "Bad Status: "++(String.fromInt status), Cmd.none)
        Http.BadUrl string ->
          (Failure <| "Bad Url: "++string, Cmd.none)


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        TryAgainPlease ->
            (Loading, getGreeting)

        GreetingResult result ->
            case result of
                Ok greeting -> (Succes greeting, Cmd.none)
                Err error -> handleError error

        SaveStuff greeter ->
            (Loading, saveGreeting greeter)

        SaveResult result ->
            case result of
                Ok msg -> (Saving msg, Cmd.none)
                Err error -> handleError error

getGreeting : Cmd Message
getGreeting = Http.get
    { url = "http://localhost:4711/greeter"
    , expect = Http.expectJson GreetingResult greeterDecoder
    }


saveGreeting : Greeter -> Cmd Message
saveGreeting greeter = Http.post
    { url = "http://localhost:4711/saveGreeter"
    , body = Http.jsonBody (encodeGreeter greeter)
    , expect = Http.expectString SaveResult
    }

{-
streetDecoder: Decoder String
streetDecoder =
    field "address" (field "street" string)
addressDecoder: Decoder Address
addressDecoder =
    map2 Address
        (field "street" string)
        (field "city" string)
greeterDecoder : Decoder Greeter
greeterDecoder =
    map4 Greeter
        (field "name" string)
        (field "greeting" string)
        (field "age" int)
        (field "address" addressDecoder)
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
-}

view : Model -> Html Message
view model =
    case model of
        Waiting -> button [ onClick TryAgainPlease ] [ text "Click for greeting"]
        Saving msg -> text ("... saving "++msg++"...")
        Failure msg -> text ("Something went wrong: "++msg)
        Loading -> text "... please wait ..."
        Succes greeter ->
            div []
                [ text ("The greeting from "++greeter.name++" who is "++(String.fromInt greeter.age)++" years old, was: "++greeter.greeting)
                , text (" it lives at "++greeter.address.street++" in "++greeter.address.city)
                , button [ onClick TryAgainPlease ] [ text "Click for new greeting" ]
                , button [ onClick (SaveStuff greeter)] [ text "Save Stuff" ]
                ]



subscriptions : Model -> Sub Message
subscriptions _ = Sub.none