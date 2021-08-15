module PixiExamples.PatternMatching exposing (..)

-- Record
type alias Person = {
    name: String ,
     age: Int}

personToString : Person -> String
personToString {name , age} = 
    "Name: " ++ name ++ ", Age: " ++ String.fromInt age

-- tuples
type alias Coordinat2D = (Float , Float)

coordinatToString : Coordinat2D -> String
coordinatToString (x , y) = 
    "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"

--Getting through pattern matching (getter function)

getPersonByName : List Person -> String -> Maybe Person
getPersonByName list name = 
    case list of
    [] -> Nothing
    head :: tail ->
        if head.name == name then
            Just head
        else
            getPersonByName tail name

