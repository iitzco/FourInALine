{-
Contains Model, Update, View
-}

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp
import Debug

--API

just : a -> Maybe a
just a = (Just a)

unJust : Maybe a -> a
unJust (Just a ) = a

type GTree a = Node a (List (GTree a))

type alias Matrix a = List (List a)

bottom = bottom

getL : List a -> Int -> a
getL l n = 
    case l of
        [] -> bottom
        x::xs -> if (n == 1) then x else getL xs (n-1) 

--return first appearence
getLIndex : List a -> a -> Int
getLIndex l elem = getLIndexR l elem 1

getLIndexR : List a -> a -> Int -> Int
getLIndexR l elem pos = 
    case l of
        [] -> bottom
        x::xs -> if (x == elem) then pos else getLIndexR xs elem (pos+1)

containsL : List a -> a -> Bool
containsL l elem =
    case l of
        [] -> False
        x::xs -> if (x == elem) then True else containsL xs elem

putLFirst : List a -> a -> a -> List a
putLFirst l first elem = 
    case l of
        [] -> []
        x::xs -> if (x == first) then elem :: xs else x :: (putLFirst xs first elem)

putL : List a -> a -> Int -> List a
putL l a n = 
    case l of
        [] -> []
        x::xs -> if (n == 1) then (a :: xs) else x :: putL xs a (n-1)


getM : Matrix a -> Int -> Int -> a
getM m r c = getL (getL m r) c

putM : Matrix a -> Int -> Int -> a -> Matrix a
putM m r c elem = 
    case m of
        [] -> bottom
        x::xs -> if (r == 1) then (putL x elem c) :: xs else x :: putM xs (r-1) c elem 


emptyMatrix : Int -> Int -> a -> (Matrix a)
emptyMatrix row col elem = List.repeat row (List.repeat col elem)

--AI

prof : Int
prof = 2

inf : Int
inf = 1000

type alias Info =
    {
    move : Int,
    heuristic : Int
    }

initInfo : Int -> Int -> Info
initInfo heuristicParam moveParam = { move = moveParam , heuristic = heuristicParam }

type alias State = 
    {
    model : Model,
    info : Info
    }

ai : Model -> Info
ai model = minimax (generateState 0 model) prof

generateState : Int -> Model -> State
generateState moveParam modelParam = 
                {
                model = modelParam,
                info = initInfo 0 moveParam
                }

generateTree : Int -> State -> GTree State
generateTree p s = if (p/=prof && (p == 0 || s.model.status/=InGame)) then (Node s []) else (Node s (List.map (generateTree (p-1)) (getStates s)))


getStates : State -> List State
getStates s = List.map (\pilar -> generateState pilar (makeMove s.model pilar)) (List.filter (\pilar -> not (isFull (getL s.model.board pilar))) [1,2,3,4,5,6,7]) 

minimax : State -> Int -> Info
minimax s p = (getHead (minimaxR p (generateTree p s))).info
--minimax s p = (getHead (minimaxR p (Debug.watch "Tree" (generateTree p s)))).info


minimaxR : Int -> GTree State -> GTree State
minimaxR p (Node s l) = 
    let i = 
        if (p/=prof && (p == 0 ||  s.model.status /= InGame))
            then initInfo (getHeuristic s) s.info.move
            else
                if (s.model.turn == unJust s.model.ai)
                    then
                        --getMax (List.map (minimaxR (p-1)) l) s.info.move
                        Debug.watch "MAX" (getMax (List.map (minimaxR (p-1)) l))
                        --getMax ( Debug.watch "ListMax" (List.map (minimaxR (p-1)) l)) s.info.move                   
                    else
                        --getMin (List.map (minimaxR (p-1)) l) s.info.move
                        Debug.watch "MIN" (getMin (List.map (minimaxR (p-1)) l))
                        --getMin ( Debug.watch "ListMin" (List.map (minimaxR (p-1)) l)) s.info.move
    in Node { s | info <- i} l

getHeuristic : State -> Int
getHeuristic s = getH1 s

--getMax : List (GTree State) -> Int -> Info
--getMax l m = 
--    case l of
--        [] -> initInfo ((-1) * (inf+1)) 1
--        x::xs -> let max = getMax xs m in if ((getHead x).info.heuristic >= max.heuristic) then initInfo (getHead x).info.heuristic m  else max

--getMin : List (GTree State) -> Int -> Info
--getMin l m = 
--    case l of
--        [] -> initInfo (inf+1) 1
--        x::xs -> let min = getMin xs m in if ((getHead x).info.heuristic <= min.heuristic) then initInfo (getHead x).info.heuristic m  else min

getMax : List (GTree State) ->  Info
getMax l  = 
    case l of
        [] -> initInfo ((-1) * (inf+1)) 0
        x::xs -> let max = getMax xs in if ((getHead x).info.heuristic >= max.heuristic) then initInfo (getHead x).info.heuristic (getHead x).info.move  else max

getMin : List (GTree State) -> Info
getMin l = 
    case l of
        [] -> initInfo (inf+1) 0
        x::xs -> let min = getMin xs in if ((getHead x).info.heuristic <= min.heuristic) then initInfo (getHead x).info.heuristic (getHead x).info.move  else min


getHead : GTree State -> State
getHead (Node s l) = s

--HEURISTICS


--only checks wins

getH1 : State -> Int 
getH1 s = if (s.model.status == Won)
            then if (s.model.turn /= unJust s.model.ai)
                then inf
                --Sometimes it gets here, sometimes not
                else (-1*inf)
            else 0

getH2 s = if (s.model.turn /= unJust s.model.ai)
            then 0
            else 10

-- MODEL

type Coin = Black | White

type Status = Starting | InGame | Won | Tied

type alias Turn = Coin

type alias Board = Matrix (Maybe Coin)

type alias Model = 
    {
    board : Board,
    turn : Turn,
    status : Status,
    coins : Int,
    ai : Maybe Coin
    }

-- ai (artificial intelligence) can be: Nothing (2 human players, no CPU) or Just c (CPU moves c)

init : Model
init = { board = getEmptyBoard , turn = Black , status = Starting, coins = 0, ai = Just White }


{-6x7 board:
    xxxxxxx
    xxxxxxx
    xxxxxxx
    xxxxxxx
    xxxxxxx
    xxxxxxx
    
 For this model is easier 7x6 (each row is a pilar of the board)
-}

getEmptyBoard : Board
getEmptyBoard = emptyMatrix 7 6 Nothing


getStringBoard : Board -> Int -> Int -> String
getStringBoard b row col =
    formString (getM b row col)

formString : Maybe Coin -> String
formString c =
    case c of
        Nothing -> "  -  "
        Just c -> case c of
                    Black -> "  B  "
                    White -> "  W  " 

getStringTurn : Turn -> String
getStringTurn t = 
    case t of
        Black -> "Turn: Black"
        White -> "Turn: White"


-- UPDATE

type Action = Move Int | Start | Replay

update : Action -> Model -> Model
update action model = 
    case action of
        Start -> {model | status <- InGame}
        Replay -> init
        Move pilar -> makeMove model pilar

makeMove : Model -> Int -> Model
makeMove model pilar = 
    let listPilar = getL model.board pilar
            in
                if (isFull listPilar) then
                    model
                else
                    let index = getLIndex listPilar Nothing
                        coinsAux = model.coins + 1
                    in 
                        {
                        model   | board <- updateBoard model.board model.turn pilar index,
                                  turn <- updateTurn model.turn,
                                  coins <- coinsAux,
                                  status <- 
                                    if (analyzeState model.board model.turn pilar index) 
                                        then 
                                            Won 
                                        else
                                           if (tiedGame coinsAux) then Tied else InGame
                        }

updateTurn : Turn -> Turn
updateTurn t = if (t==Black) then White else Black

updateBoard : Board -> Turn -> Int -> Int -> Board
updateBoard b t pilar index = putM b pilar index (Just t)

isFull : List (Maybe Coin) -> Bool
isFull l = not (containsL l Nothing)


--TODO Analizar que pasa si pongo en el medio...
analyzeState : Board -> Turn -> Int -> Int -> Bool
analyzeState b t pilar index = containsL (List.map followPath   [(b,t,pilar,index,1),
                                                                 (b,t,pilar,index,2),
                                                                 (b,t,pilar,index,3),
                                                                 (b,t,pilar,index,4),
                                                                 (b,t,pilar,index,5),
                                                                 (b,t,pilar,index,6),
                                                                 (b,t,pilar,index,7),
                                                                 (b,t,pilar,index,8)]) True

followPath : (Board,Turn,Int,Int,Int) -> Bool
followPath (b,t,pilar,index,path) = followPathR b t pilar index path 4

followPathR : Board -> Turn -> Int -> Int -> Int -> Int -> Bool
followPathR b t pilar index path remain = 
    let index2 = 
            case path of
                1 -> index + 1
                2 -> index + 1
                4 -> index - 1
                5 -> index - 1
                6 -> index - 1
                8 -> index + 1
                _ -> index
        pilar2 = 
            case path of
                2 -> pilar + 1
                3 -> pilar + 1
                4 -> pilar + 1
                6 -> pilar - 1
                7 -> pilar - 1
                8 -> pilar - 1
                _ -> pilar
        remain2 = remain - 1
    in 
        if (pilar2 < 1 || pilar2 > 7 || index2 < 1 || index2 > 6)
            then
                False
            else
                if (getM b pilar2 index2 /= (Just t))
                    then False
                    else 
                        if (remain2 == 1 || (remain2 == 2 && opposite b t pilar2 index2 path))
                            then True
                            else
                                followPathR b t pilar2 index2 path remain2

opposite : Board -> Turn -> Int -> Int -> Int -> Bool
opposite b t pilar index path =
    let index2 = 
            case path of
                1 -> index - 3
                2 -> index - 3
                4 -> index + 3
                5 -> index + 3
                6 -> index + 3
                8 -> index - 3
                _ -> index
        pilar2 = 
            case path of
                2 -> pilar - 3
                3 -> pilar - 3
                4 -> pilar - 3
                6 -> pilar + 3
                7 -> pilar + 3
                8 -> pilar + 3
                _ -> pilar
    in 
        if (pilar2 < 1 || pilar2 > 7 || index2 < 1 || index2 > 6 || getM b pilar2 index2 /= (Just t))
            then False
            else True


tiedGame : Int -> Bool
tiedGame n = (n == (7*6))


-- VIEW

inline : Attribute
inline =
  style
    [ ("display", "inline-block"),
      ("width","20px"),
      ("margin-left","20px"),
      ("margin-right","20px"),
      ("text-align","center")
    ]



-- The idea is to manage AI reponse through the view simulating a "virtual player"

view : Signal.Address Action -> Model -> Html
view address model = 
    case model.status of
        Starting ->
            getViewStart address model
        InGame ->
            if ( model.ai == just model.turn) 
                then 
                    getViewInGameCPU address model
                else 
                    getViewInGameHuman address model
        Won ->
            getViewWon address model
        Tied ->
            getViewTied address model
     
getViewStart : Signal.Address Action -> Model -> Html
getViewStart address model = div [] [button [onClick address (Start)] [ text "Play!" ]]

getViewInGameHuman : Signal.Address Action -> Model -> Html
getViewInGameHuman address model = 
        div []
            [ 
            h1 [] [text (getStringTurn model.turn)],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 1 6)],
                h2 [] [text (getStringBoard model.board 1 5)],
                h2 [] [text (getStringBoard model.board 1 4)],
                h2 [] [text (getStringBoard model.board 1 3)],
                h2 [] [text (getStringBoard model.board 1 2)],
                h2 [] [text (getStringBoard model.board 1 1)],
                button [onClick address (Move 1)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 2 6)],
                h2 [] [text (getStringBoard model.board 2 5)],
                h2 [] [text (getStringBoard model.board 2 4)],
                h2 [] [text (getStringBoard model.board 2 3)],
                h2 [] [text (getStringBoard model.board 2 2)],
                h2 [] [text (getStringBoard model.board 2 1)],
                button [onClick address (Move 2)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 3 6)],
                h2 [] [text (getStringBoard model.board 3 5)],
                h2 [] [text (getStringBoard model.board 3 4)],
                h2 [] [text (getStringBoard model.board 3 3)],
                h2 [] [text (getStringBoard model.board 3 2)],
                h2 [] [text (getStringBoard model.board 3 1)],
                button [onClick address (Move 3)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 4 6)],
                h2 [] [text (getStringBoard model.board 4 5)],
                h2 [] [text (getStringBoard model.board 4 4)],
                h2 [] [text (getStringBoard model.board 4 3)],
                h2 [] [text (getStringBoard model.board 4 2)],
                h2 [] [text (getStringBoard model.board 4 1)],
                button [onClick address (Move 4)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 5 6)],
                h2 [] [text (getStringBoard model.board 5 5)],
                h2 [] [text (getStringBoard model.board 5 4)],
                h2 [] [text (getStringBoard model.board 5 3)],
                h2 [] [text (getStringBoard model.board 5 2)],
                h2 [] [text (getStringBoard model.board 5 1)],
                button [onClick address (Move 5)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 6 6)],
                h2 [] [text (getStringBoard model.board 6 5)],
                h2 [] [text (getStringBoard model.board 6 4)],
                h2 [] [text (getStringBoard model.board 6 3)],
                h2 [] [text (getStringBoard model.board 6 2)],
                h2 [] [text (getStringBoard model.board 6 1)],
                button [onClick address (Move 6)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 7 6)],
                h2 [] [text (getStringBoard model.board 7 5)],
                h2 [] [text (getStringBoard model.board 7 4)],
                h2 [] [text (getStringBoard model.board 7 3)],
                h2 [] [text (getStringBoard model.board 7 2)],
                h2 [] [text (getStringBoard model.board 7 1)],
                button [onClick address (Move 7)] [ text "-" ]
                ]
            ]

getViewInGameCPU : Signal.Address Action -> Model -> Html
getViewInGameCPU address model = 
    let info = ai model
    in
    -- TODO
        div []
            [ 
            div [] [h2 [] [text "Thinking..."],
                    button [onClick address (Move info.move)] [ text "Think!" ],
                    div [inline] [h2 [] [text (toString info.heuristic)]],
                    div [inline] [h2 [] [text (toString info.move)]]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 1 6)],
                h2 [] [text (getStringBoard model.board 1 5)],
                h2 [] [text (getStringBoard model.board 1 4)],
                h2 [] [text (getStringBoard model.board 1 3)],
                h2 [] [text (getStringBoard model.board 1 2)],
                h2 [] [text (getStringBoard model.board 1 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 2 6)],
                h2 [] [text (getStringBoard model.board 2 5)],
                h2 [] [text (getStringBoard model.board 2 4)],
                h2 [] [text (getStringBoard model.board 2 3)],
                h2 [] [text (getStringBoard model.board 2 2)],
                h2 [] [text (getStringBoard model.board 2 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 3 6)],
                h2 [] [text (getStringBoard model.board 3 5)],
                h2 [] [text (getStringBoard model.board 3 4)],
                h2 [] [text (getStringBoard model.board 3 3)],
                h2 [] [text (getStringBoard model.board 3 2)],
                h2 [] [text (getStringBoard model.board 3 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 4 6)],
                h2 [] [text (getStringBoard model.board 4 5)],
                h2 [] [text (getStringBoard model.board 4 4)],
                h2 [] [text (getStringBoard model.board 4 3)],
                h2 [] [text (getStringBoard model.board 4 2)],
                h2 [] [text (getStringBoard model.board 4 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 5 6)],
                h2 [] [text (getStringBoard model.board 5 5)],
                h2 [] [text (getStringBoard model.board 5 4)],
                h2 [] [text (getStringBoard model.board 5 3)],
                h2 [] [text (getStringBoard model.board 5 2)],
                h2 [] [text (getStringBoard model.board 5 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 6 6)],
                h2 [] [text (getStringBoard model.board 6 5)],
                h2 [] [text (getStringBoard model.board 6 4)],
                h2 [] [text (getStringBoard model.board 6 3)],
                h2 [] [text (getStringBoard model.board 6 2)],
                h2 [] [text (getStringBoard model.board 6 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 7 6)],
                h2 [] [text (getStringBoard model.board 7 5)],
                h2 [] [text (getStringBoard model.board 7 4)],
                h2 [] [text (getStringBoard model.board 7 3)],
                h2 [] [text (getStringBoard model.board 7 2)],
                h2 [] [text (getStringBoard model.board 7 1)]]]  

getViewWon : Signal.Address Action -> Model -> Html
getViewWon address model = 
        div []
            [ 
            div [] [h2 [] [text "Won"],h2 [] [text (getStringTurn model.turn)]],
            button [onClick address (Replay)] [ text "Replay" ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 1 6)],
                h2 [] [text (getStringBoard model.board 1 5)],
                h2 [] [text (getStringBoard model.board 1 4)],
                h2 [] [text (getStringBoard model.board 1 3)],
                h2 [] [text (getStringBoard model.board 1 2)],
                h2 [] [text (getStringBoard model.board 1 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 2 6)],
                h2 [] [text (getStringBoard model.board 2 5)],
                h2 [] [text (getStringBoard model.board 2 4)],
                h2 [] [text (getStringBoard model.board 2 3)],
                h2 [] [text (getStringBoard model.board 2 2)],
                h2 [] [text (getStringBoard model.board 2 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 3 6)],
                h2 [] [text (getStringBoard model.board 3 5)],
                h2 [] [text (getStringBoard model.board 3 4)],
                h2 [] [text (getStringBoard model.board 3 3)],
                h2 [] [text (getStringBoard model.board 3 2)],
                h2 [] [text (getStringBoard model.board 3 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 4 6)],
                h2 [] [text (getStringBoard model.board 4 5)],
                h2 [] [text (getStringBoard model.board 4 4)],
                h2 [] [text (getStringBoard model.board 4 3)],
                h2 [] [text (getStringBoard model.board 4 2)],
                h2 [] [text (getStringBoard model.board 4 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 5 6)],
                h2 [] [text (getStringBoard model.board 5 5)],
                h2 [] [text (getStringBoard model.board 5 4)],
                h2 [] [text (getStringBoard model.board 5 3)],
                h2 [] [text (getStringBoard model.board 5 2)],
                h2 [] [text (getStringBoard model.board 5 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 6 6)],
                h2 [] [text (getStringBoard model.board 6 5)],
                h2 [] [text (getStringBoard model.board 6 4)],
                h2 [] [text (getStringBoard model.board 6 3)],
                h2 [] [text (getStringBoard model.board 6 2)],
                h2 [] [text (getStringBoard model.board 6 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 7 6)],
                h2 [] [text (getStringBoard model.board 7 5)],
                h2 [] [text (getStringBoard model.board 7 4)],
                h2 [] [text (getStringBoard model.board 7 3)],
                h2 [] [text (getStringBoard model.board 7 2)],
                h2 [] [text (getStringBoard model.board 7 1)]]]    


getViewTied : Signal.Address Action -> Model -> Html
getViewTied address model = 
        div []
            [
            div [] [h2 [] [text "Tied"]],
            button [onClick address (Replay)] [ text "Replay" ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 1 6)],
                h2 [] [text (getStringBoard model.board 1 5)],
                h2 [] [text (getStringBoard model.board 1 4)],
                h2 [] [text (getStringBoard model.board 1 3)],
                h2 [] [text (getStringBoard model.board 1 2)],
                h2 [] [text (getStringBoard model.board 1 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 2 6)],
                h2 [] [text (getStringBoard model.board 2 5)],
                h2 [] [text (getStringBoard model.board 2 4)],
                h2 [] [text (getStringBoard model.board 2 3)],
                h2 [] [text (getStringBoard model.board 2 2)],
                h2 [] [text (getStringBoard model.board 2 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 3 6)],
                h2 [] [text (getStringBoard model.board 3 5)],
                h2 [] [text (getStringBoard model.board 3 4)],
                h2 [] [text (getStringBoard model.board 3 3)],
                h2 [] [text (getStringBoard model.board 3 2)],
                h2 [] [text (getStringBoard model.board 3 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 4 6)],
                h2 [] [text (getStringBoard model.board 4 5)],
                h2 [] [text (getStringBoard model.board 4 4)],
                h2 [] [text (getStringBoard model.board 4 3)],
                h2 [] [text (getStringBoard model.board 4 2)],
                h2 [] [text (getStringBoard model.board 4 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 5 6)],
                h2 [] [text (getStringBoard model.board 5 5)],
                h2 [] [text (getStringBoard model.board 5 4)],
                h2 [] [text (getStringBoard model.board 5 3)],
                h2 [] [text (getStringBoard model.board 5 2)],
                h2 [] [text (getStringBoard model.board 5 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 6 6)],
                h2 [] [text (getStringBoard model.board 6 5)],
                h2 [] [text (getStringBoard model.board 6 4)],
                h2 [] [text (getStringBoard model.board 6 3)],
                h2 [] [text (getStringBoard model.board 6 2)],
                h2 [] [text (getStringBoard model.board 6 1)]],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 7 6)],
                h2 [] [text (getStringBoard model.board 7 5)],
                h2 [] [text (getStringBoard model.board 7 4)],
                h2 [] [text (getStringBoard model.board 7 3)],
                h2 [] [text (getStringBoard model.board 7 2)],
                h2 [] [text (getStringBoard model.board 7 1)]]]

-- main

main = StartApp.start {model = init, update = update, view = view}
