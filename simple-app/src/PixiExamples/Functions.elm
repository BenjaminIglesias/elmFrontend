module PixiExamples.Functions exposing (..)

-- Functions As First Class Citizens
-- Means that functions are treated as any other datatype, they can be stored
-- in variables, passed as arguments, returned from functions and stored in datastructures

function = \a -> \b -> a + b

-- in datastructure
var listOfFuncs = [(\a -> a * 2), (\a -> a + 100)]

--Higher Order Functions
--Subconcept of first class citizenship
--Means that functions can be passed as arguments and be returned from functions

doOnEachElement : List number -> List number
doOnEachElement list =
    List.map (function 5) list

--Lambdas (Anonymous Functions)
-- Used when u only need a functions functionality once
-- An example is the map func

doOnEachElementV2 : List number -> List Number
doOnEachElementV2 list = 
    List.map (\a -> (30 * a + 300 /(a / 3)))

