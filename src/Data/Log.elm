module Data.Log exposing (Log, toString)

type alias Log =
    { content : String
    }

toString: Log -> String
toString log = log.content