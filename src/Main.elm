module Main exposing (..)

import Browser
import Browser exposing (Document)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Data.ClassicCardSet as ClassicCardSet exposing (..)
import Data.Card as Card exposing (Card)
import Data.Turn as Turn exposing (Turn)
import Data.Player as Player exposing (Player)
import Data.Log as Log exposing (Log)
import Random
import Html.Attributes exposing (disabled)

maxIter : Int
maxIter = 1000

main : Program () Model Msg
main = 
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model = 
    { playerA : Player
    , playerB : Player
    , turns : List Turn
    , status : Status
    }

type Status = 
    Ongoing
    | HasWinner Player
    | Tie
    | MaxIterReached

setPlayerA : Player -> Model -> Model
setPlayerA player model = 
    { model | playerA = player }

setPlayerB : Player-> Model -> Model
setPlayerB player model = 
    { model | playerB = player }

asPileIn : Player -> List Card -> Player
asPileIn player newPile = 
    { player | pile = newPile }

init : () -> (Model, Cmd Msg)
init _ = 
    ( Model (Player "Player A" []) (Player "Player B" []) [] Ongoing
    , shuffleThenDeal
    )

shuffleThenDeal : Cmd Msg
shuffleThenDeal = 
    Random.generate Deal (ClassicCardSet.shuffle ClassicCardSet.new)

type Msg
    = Deal ClassicCardSet
    | SimulateTurn
    | SimulateAllTurns
    | GotTurn Turn Player Status Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Deal cardSet ->
            let
                ( cardsA, cardsB ) = ClassicCardSet.split cardSet
                playerA =
                    cardsA
                    |> asPileIn model.playerA
                playerB =
                    cardsB
                    |> asPileIn model.playerB
                newModel = model
                    |> setPlayerA playerA
                    |> setPlayerB playerB
            in
                ( newModel, Cmd.none )
        SimulateTurn ->
            simulateTurn model False
        SimulateAllTurns -> 
            simulateAllTurns model
        GotTurn turn winner status continue ->
            let
                turnWithLogs = generateLogs winner status turn
                newTurns = model.turns ++ [turnWithLogs]
                newModel = { model | turns = newTurns, status = status, playerA = turn.playerA, playerB = turn.playerB }
            in
                if continue then
                    simulateAllTurns newModel
                else
                    ( newModel, Cmd.none )

simulateAllTurns : Model -> (Model, Cmd Msg)
simulateAllTurns model = 
    case model.status of
        Ongoing -> simulateTurn model True
        _ -> ( model, Cmd.none )

simulateTurn : Model -> Bool -> (Model, Cmd Msg)
simulateTurn model continue = 
    let
        i = List.length model.turns
        { turn, gameStatus, winner } = Turn.resolveTurn model.playerA model.playerB i
        winnerGenerator = Player.takeCards winner turn.stagingCards
        newStatus = 
            case gameStatus of
                Turn.HasGameWinner -> HasWinner winner
                Turn.TieGame -> Tie
                Turn.GameNotFinished -> if i >= maxIter - 1 then MaxIterReached else Ongoing
    in
        if winner.name == model.playerA.name then
            (model, Random.generate (\winnerWithTakenCards -> GotTurn { turn | playerA = winnerWithTakenCards } winner newStatus continue) winnerGenerator)
        else
            (model, Random.generate (\winnerWithTakenCards -> GotTurn { turn | playerB = winnerWithTakenCards } winner newStatus continue) winnerGenerator)

generateLogs : Player -> Status -> Turn -> Turn
generateLogs winner status oldTurn = 
    let
        -- Log winner of turn
        index = oldTurn.index
        winnerOfTurnLogs = [ Log ("\t# \\o/ Player \"" ++ (Player.toString winner) ++ "\" wins the turn") ]
        endOfTurnLogs = 
            case status of
                HasWinner _ ->
                    [ Log ("\tEnd of turn " ++ (String.fromInt index) ++ "; the game stops immediatly because a player has only one or zero remaining card, he is the loser")
                    , Log "" ]
                Tie ->
                    [ Log ("\tEnd of turn " ++ (String.fromInt index) ++ "; the game stops immediatly because both players have only one or zero remaining card, it is a tie game")
                    , Log "" ]
                _ ->
                    [ Log ("\tEnd of turn " ++ (String.fromInt index) ++ "; the following cards will be taken by the winner and put randomly at the bottom of his pile => "
                        ++ (Turn.prettyPrintCards oldTurn.stagingCards))
                    , Log ("\tPlayer \"" ++ (Player.toString oldTurn.playerA) ++ "\" has " ++ (String.fromInt (List.length oldTurn.playerA.pile)) ++ " cards remaining")
                    , Log ("\tPlayer \"" ++ (Player.toString oldTurn.playerB) ++ "\" has " ++ (String.fromInt (List.length oldTurn.playerB.pile)) ++ " cards remaining")
                    , Log ""
                    ]

    in
        { oldTurn | logs = oldTurn.logs ++ winnerOfTurnLogs ++ endOfTurnLogs }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Document Msg
view model = 
    let
        body = div []
            [ playersPanel model
            , controlPanel model
            ]
    in
        Document
            "War Card Game"
            [ body ]

controlPanel : Model -> Html Msg
controlPanel model = div [ class "column" ]
    [ row (button [ onClick SimulateTurn, disabled (model.status /= Ongoing) ] [text "Next turn"])
    , row (button [ onClick SimulateAllTurns, disabled (model.status /= Ongoing) ] [text "All turns until the end"])
    , row (h3 [] [ text "Logs" ])
    , row (div [] (logs model))
    ]

logs : Model -> List (Html Msg)
logs model = 
    let
        intro = 
            [ Log "Hello, welcome to the War Card Game"
            , Log ""
            ]
        turns = 
            model.turns
                |> List.map .logs
                |> List.concat
        outro = 
            case model.status of
                MaxIterReached -> [Log ("No winner, the game exceeded the max number of allowed turns (" ++ (String.fromInt maxIter) ++ ")") ]
                HasWinner w -> [ Log ("And the winner is... " ++ (Player.toString w) ++ " ! Congratulations !") ]
                Tie -> [ Log ("And the winner is... No winner! Tie game !") ]
                _ -> []
    in
        (intro ++ turns ++ outro)
            |> logToHtml

logToHtml : List Log -> List (Html Msg)
logToHtml xs = 
    xs
    |> List.map Log.toString
    |> List.map (\s -> if String.isEmpty s then "#" else s)
    |> List.map text
    |> List.map row

row : Html msg -> Html msg
row el = 
    div [ class "row" ] [ el ]

playersPanel : Model -> Html msg
playersPanel model = 
    div [ class "column"] [ playerAsDiv model.playerA, playerAsDiv model.playerB ]

playerAsDiv : Player -> Html msg
playerAsDiv player = 
    div []
        [ h2 [] [ text (player.name ++ " (" ++ String.fromInt(List.length player.pile) ++ ")") ]
        , ul [] (cardsAsListEl player.pile)
        ]

cardsAsListEl : List Card -> List (Html msg)
cardsAsListEl cards = 
    cards
    |> List.map Card.toString
    |> List.map text
    |> List.map (\el -> li [] [el])