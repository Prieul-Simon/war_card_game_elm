module Data.Turn exposing (Turn, TurnResult, GameStatus(..), resolveTurn, prettyPrintCards)

import Data.Player as Player exposing (Player, ResultOfWar(..))
import Data.Log exposing (Log)
import Data.Card as Card exposing (Card)

type alias Turn = 
    { index : Int
    , playerA : Player
    , playerB : Player
    , stagingCards : List Card
    , logs : List Log
    }

resolveTurn : Player -> Player -> Int -> TurnResult
resolveTurn playerA playerB index = 
    let
        -- First, play "normally"
        (maybeCardA, newPlayerA) = Player.play playerA
        (maybeCardB, newPlayerB) = Player.play playerB
    in
        case (maybeCardA, maybeCardB) of
            (Nothing, Nothing) -> TurnResult (Turn index newPlayerA newPlayerB [] []) TieGame newPlayerB
            (Nothing, Just cardB) -> TurnResult (Turn index newPlayerA newPlayerB [ cardB ] []) HasGameWinner newPlayerB
            (Just cardA, Nothing) -> TurnResult (Turn index newPlayerA newPlayerB [ cardA ] []) HasGameWinner newPlayerA
            (Just cardA, Just cardB) ->
                -- Then determinate the winner, with potential multiple wars in a row
                generateLogs cardA cardB (compareCardsOrMakeWar cardA cardB (Turn index newPlayerA newPlayerB [ cardA, cardB ] []))

generateLogs : Card -> Card -> TurnResult -> TurnResult
generateLogs cardA cardB result = 
    let
        -- Log revealed cards
        oldTurn = result.turn
        index = oldTurn.index
        initialLogs =
            [ Log ("~~~~~ Turn " ++ (String.fromInt index) ++ " ~~~~~")
            , Log ("\tPlayer \"" ++ (Player.toString oldTurn.playerA) ++ "\" reveals the top card \"" ++ (Card.toString cardA) ++ "\"")
            , Log ("\tPlayer \"" ++ (Player.toString oldTurn.playerB) ++ "\" reveals the top card \"" ++ (Card.toString cardB) ++ "\"")
            ]

        newTurn = { oldTurn | logs = initialLogs ++ oldTurn.logs }
    in
        { result | turn = newTurn }

compareCardsOrMakeWar : Card -> Card -> Turn -> TurnResult
compareCardsOrMakeWar cardA cardB turn = 
    if Card.isEqual cardA cardB then
        -- LET'S MAKE WAR
        makeWar (addLog (Log "\tCards are equal => WAR") turn)
    else if Card.isHigher cardA cardB then
        -- Player A wins the turn
        TurnResult turn (if Player.hasLostGame turn.playerB then HasGameWinner else GameNotFinished) turn.playerA
    else
        -- Player B wins the turn
        TurnResult turn (if Player.hasLostGame turn.playerA then HasGameWinner else GameNotFinished) turn.playerB

makeWar : Turn -> TurnResult
makeWar turn = 
    let
        (resultA, newPlayerA) = Player.resultOfWar turn.playerA
        (resultB, newPlayerB) = Player.resultOfWar turn.playerB
    in
        case (resultA, resultB) of
            (InstantLose, InstantLose) -> 
                TurnResult 
                    (addLog (Log ("\tBoth players have only one or zero card remaining => TIE GAME")) turn)
                    TieGame
                    newPlayerB
            (InstantLose, _) -> 
                TurnResult 
                    (addLog (Log ("\tPlayer \"" ++ (Player.toString newPlayerA) ++ "\" has only one or zero card remaining => INSTANT LOSE")) turn)
                    HasGameWinner
                    newPlayerB
            (_, InstantLose) ->
                TurnResult
                    (addLog (Log ("\tPlayer \"" ++ (Player.toString newPlayerB) ++ "\" has only one or zero card remaining => INSTANT LOSE")) turn)
                    HasGameWinner
                    newPlayerA
            (ContinueGame continueGameA, ContinueGame continueGameB) -> 
                let
                    stagingCards = turn.stagingCards ++ [continueGameA.faceDown, continueGameB.faceDown, continueGameA.faceUp, continueGameB.faceUp]
                    finalTurn = turn
                        |> setStagingCards stagingCards
                        |> setPlayerA newPlayerA
                        |> setPlayerB newPlayerB
                        |> addLog (Log ("\tPlayer \"" ++ (Player.toString newPlayerA) ++ "\" places face down the card \"" ++ (Card.toString continueGameA.faceDown) ++ "\""))
                        |> addLog (Log ("\tPlayer \"" ++ (Player.toString newPlayerB) ++ "\" places face down the card \"" ++ (Card.toString continueGameB.faceDown) ++ "\""))
                        |> addLog (Log ("\tPlayer \"" ++ (Player.toString newPlayerA) ++ "\" places face up the card \"" ++ (Card.toString continueGameA.faceUp) ++ "\" and is ready for combat"))
                        |> addLog (Log ("\tPlayer \"" ++ (Player.toString newPlayerB) ++ "\" places face up the card \"" ++ (Card.toString continueGameB.faceUp) ++ "\" and is ready for combat"))
                in
                    compareCardsOrMakeWar continueGameA.faceUp continueGameB.faceUp finalTurn

type alias TurnResult =
    { turn : Turn
    , gameStatus : GameStatus
    , winner : Player
    }

type GameStatus
    = GameNotFinished
    | HasGameWinner
    | TieGame

setPlayerA : Player -> Turn -> Turn
setPlayerA p turn = { turn | playerA = p }

setPlayerB : Player -> Turn -> Turn
setPlayerB p turn = { turn | playerB = p }

setStagingCards : List Card -> Turn -> Turn
setStagingCards cards turn = { turn | stagingCards = cards }

addLog : Log -> Turn -> Turn
addLog log turn = 
    { turn | logs = (turn.logs ++ [log]) }

prettyPrintCards : List Card -> String
prettyPrintCards cards =
    let
        joined = cards
            |> List.map Card.toString
            |> String.join ", "
    in
        "[" ++ joined ++ "]"