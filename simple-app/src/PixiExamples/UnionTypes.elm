module PixiExamples.UnionTypes exposing (..)
import Http

-- Union is a value that can have any of several formats within the same position in memory. Meaning a variable will be permited to have different types stored in its instances.
-- Let say we have a chatroom. Everyone will need a name, but some users might have permenant accounts and some might be visitors. 

type UserStatus
    = Regular
    | Visitor

type alias User =
  { status : UserStatus
  , name : String
  }

thomas = { status = Regular, name = "Thomas" }
kate95 = { status = Visitor, name = "kate95" }

getUserStatus : User -> String
getUserStatus user = 
    case user.status of 
        Regular ->
            "Regular user: " ++ user.name ++ "is online"
        Visitor -> 
            "Visitor user: " ++ user.name ++ "is online"

-- In a situation where a value can be set to "null" (dosent exist in FP), we can use the union type called Maybe

type Maybe x
    = Nothing
    | Just x 

-- We also use union types for errors
type alias Book =
        {title : String
        ,author : String
        }
type AddBook
    = Adding Book
    | Loading Book
    | Failure Http.Error