module Data.Card exposing (Card, toString, isEqual, isHigher, Suit(..), allSuits, Value(..), allValues)

type alias Card =
    { suit : Suit
    , value : Value
    }

toString : Card -> String
toString card = 
    (valueToString card.value) ++ " of " ++ (suitToString card.suit)

isEqual : Card -> Card -> Bool
isEqual card other = 
    card.value == other.value

isHigher : Card -> Card -> Bool
isHigher card other = 
    valueToInt card.value > valueToInt other.value

type Suit
    = Spade
    | Heart
    | Diamond
    | Club

suitToString : Suit -> String
suitToString suit =
    case suit of
        Spade -> "♠️"
        Heart -> "♥️"
        Diamond -> "♦️"
        Club -> "♣️"

allSuits : List Suit
allSuits =
    [ Spade, Heart, Diamond, Club ]

type Value
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

allValues : List Value
allValues = 
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]

valueToInt : Value -> Int
valueToInt value = 
    case value of
        Two -> 2
        Three -> 3
        Four -> 4
        Five -> 5
        Six -> 6
        Seven -> 7
        Eight -> 8
        Nine -> 9
        Ten -> 10
        Jack -> 11
        Queen -> 12
        King -> 13
        Ace -> 14

valueToString : Value -> String
valueToString value = 
    case value of
        Jack -> "J"
        Queen -> "Q"
        King -> "K"
        Ace -> "A"
        other ->
            other
            |> valueToInt
            |> String.fromInt