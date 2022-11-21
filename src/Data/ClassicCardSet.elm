module Data.ClassicCardSet exposing (cardsNumber, ClassicCardSet, new, shuffle, split)

import Data.Card exposing (Card, allSuits, allValues)
import Random.List
import Random exposing (Generator)

cardsNumber : Int
cardsNumber = 52

type alias ClassicCardSet =
    { cards : List Card
    }

new : ClassicCardSet
new =
    let
        allCardsFromSingleSuit (values, suit)
            = values
            |> List.map (\singleValue -> (suit, singleValue))
            |> List.map (\(s, v) -> Card s v)
    
    in
        allSuits                                     -- List Suit                    i.e. [ Spade, Heart, Diamond, Club ] length=4
            |> List.map (\suit -> (allValues, suit)) -- List (List Value, Suit)      i.e. [ ( [2,3,...,King,Ace], Spade ), ..., ( [2,3,...,King,Ace], Club ) ] length=4
            |> List.map allCardsFromSingleSuit       -- List List Card               i.e. [ [ (2, Spade),...,(Ace, Spade) ], ..., [ (2, Club),...,(Ace, Club) ] ] length=4 then each length=13
            |> List.concat                           -- List Card                    i.e. [ (2, Spade),...(Ace, Spade),(2, Heart),...,(Ace, Club) ] length=52
            |> ClassicCardSet                        -- a.k.a. ClassicCardSet

shuffle : ClassicCardSet -> Generator ClassicCardSet
shuffle cardSet 
    = cardSet.cards
    |> Random.List.shuffle
    |> Random.map ClassicCardSet

split : ClassicCardSet -> (List Card, List Card)
split cardSet =
    let
        half = List.length cardSet.cards // 2 -- must be 52 / 2 = 26
    in
        ( List.take half cardSet.cards
        , List.drop half cardSet.cards
        )