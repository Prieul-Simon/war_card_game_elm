module Data.Player exposing (Player, play, ResultOfWar(..), resultOfWar, takeCards, hasLostGame, toString)

import Data.Card exposing (Card)
import Random.List
import Random exposing (Generator)

type alias Player = 
    { name : String
    , pile : List Card
    }

play : Player -> (Maybe Card, Player)
play player = 
    case player.pile of
        [] -> (Nothing, player)
        head::tail -> (Just head, { player | pile = tail })

resultOfWar : Player -> (ResultOfWar, Player)
resultOfWar player = 
    case player.pile of
        [] -> (InstantLose, player)
        _::[] -> (InstantLose, player)
        -- Those two preceding cases are not impossible, this can happen if a player is engaged in a war but has no remaining cards in his pile 
        -- (but some can be staging)
        -- https://en.wikipedia.org/wiki/War_(card_game) says :
        -- 
        -- Most descriptions of War are unclear about what happens if a player runs out of cards during a war.[2] 
        -- In some variants, that player immediately loses. In others, the player may play the last card in their deck 
        -- as their face-up card for the remainder of the war or replay the game from the beginning.[2]
        -- 
        -- Here we decide to declare an "instant lose" for this player for more fun :D
        first::second::tail ->
            (ContinueGame
                { faceDown = first
                , faceUp = second
                }
            , { player | pile = tail }
            )

takeCards : Player -> List Card -> Generator Player
takeCards player cards =
    Random.List.shuffle cards
    |> Random.map (\shuffledCards -> player.pile ++ shuffledCards)
    |> Random.map (\cardsWithNewOnes -> { player | pile = cardsWithNewOnes })

hasLostGame : Player -> Bool
hasLostGame player = List.isEmpty player.pile

toString : Player -> String
toString player = player.name

type ResultOfWar
    = ContinueGame{faceDown : Card, faceUp : Card}
    | InstantLose
