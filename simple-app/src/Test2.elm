module Test2 exposing (..)

import Browser
import Http
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Events exposing (..)

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }



init : () -> (Model, Cmd Message)

init _ = ({persons = [], error = Nothing}, Cmd.none)



type alias Model =

    { persons : List Person
    , error : Maybe String
    }
type alias Person = 

    { navn : String
    , yndlingsKaffe : String
    , yndlingsSpil : String}

type Message 
 = Httprequest 
 | DataReceived (Result Http.Error (List Person))


update : Message -> Model -> (Model, Cmd Message)

update message model = 
    case message of
        Httprequest ->
            ( model, httpCommand )
        DataReceived (Ok persons) ->
            ({ model 
              | persons = persons
            , error = Nothing }, Cmd.none )
        DataReceived (Err httpError) ->
            ({ model
              | error = Just (buildError httpError)}
              , Cmd.none )


buildError : Http.Error -> String
buildError httpError = 
    case httpError of
        Http.BadUrl message ->
            message
        Http.Timeout ->
            "You got a timeout"
        Http.NetworkError ->
            "Network error"
        Http.BadStatus badStatus ->
            "Bad Status" ++ String.fromInt badStatus
        Http.BadBody badBody ->
            badBody


httpCommand : Cmd Message
httpCommand =
    Http.get
        { url = "http://localhost:5019/books"
        , expect = Http.expectJson DataReceived (list personDecoder)
        }


personDecoder : Decoder Person
personDecoder =
    map3 Person (field "navn" string) (field "yndlingsKaffe" string) (field "yndlingsSpil" string)     




view : Model -> Html Message
view model =
    div []
        [button [onClick Httprequest] [text "Klik her for at hente kaffe"], viewPersonError model]



viewPersonError : Model -> Html Message
viewPersonError model = 
    case model.error of
        Just message ->
            viewError message
        Nothing ->
            viewPersons model.persons


viewError : String -> Html Message
viewError error =
    let header = "Could not fetch" 
    in
    div [][h2[][text header], text error]

viewPerson : Person -> Html Message
viewPerson person = 
     tr[][td[][text person.navn], td[][text person.yndlingsKaffe], td[][text person.yndlingsSpil]]    

viewPersons : List Person -> Html Message
viewPersons persons =
    div []
        [ h3 [] [ text "Liste af Personer:" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewPerson persons)
        ]
viewTableHeader : Html Message
viewTableHeader =
    tr []
        [ th []
            [ text "Navn" ]
        , th []
            [ text "Yndlingskaffe" ]
        , th []
            [ text "Yndlingsspil" ]
            
        ]