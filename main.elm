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
prof = 4

inf : Int
inf = 1000

type alias Info =
    {
    move : Int,
    bestMove : Int,
    heuristic : Int
    }

initInfo : Int -> Int -> Int -> Info
initInfo heuristicParam moveParam bestMoveParam = { move = moveParam , heuristic = heuristicParam, bestMove = bestMoveParam }

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
                info = initInfo 0 moveParam 0
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
            then initInfo (getHeuristic s) s.info.move s.info.move
            else
                if (s.model.turn == unJust s.model.ai)
                    then
                        --getMax (List.map (minimaxR (p-1)) l) s.info.move
                        Debug.watch "MAX" (getMax s (List.map (minimaxR (p-1)) l)) 
                        --getMax ( Debug.watch "ListMax" (List.map (minimaxR (p-1)) l)) s.info.move                   
                    else
                        --getMin (List.map (minimaxR (p-1)) l) s.info.move
                        Debug.watch "MIN" (getMin s (List.map (minimaxR (p-1)) l))
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

getMax : State -> List (GTree State) ->  Info
getMax s l  = 
    case l of
        [] -> initInfo ((-1) * (inf+1)) 0 s.info.move
        x::xs -> let max = getMax s xs in if ((getHead x).info.heuristic >= max.heuristic) then initInfo (getHead x).info.heuristic s.info.move (getHead x).info.move  else max

getMin : State -> List (GTree State) -> Info
getMin s l = 
    case l of
        [] -> initInfo (inf+1) 0 s.info.move
        x::xs -> let min = getMin s xs in if ((getHead x).info.heuristic <= min.heuristic) then initInfo (getHead x).info.heuristic s.info.move (getHead x).info.move  else min


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
            else let h = getH2 s
                 in
                    if (s.model.turn /= unJust s.model.ai)
                        then h
                        else (-1*h)

-- checks lines of 3 
getH2 : State -> Int
getH2 s = 
    let index = getLIndex (getL s.model.board s.info.move) (just s.model.turn)
    in
        let aux = List.foldr (\b c -> if b then c + 10 else c) 0 (List.map followPath [
                                (s.model.board,s.model.turn,s.info.move, index, 1,3),
                                (s.model.board,s.model.turn,s.info.move, index, 2,3),
                                (s.model.board,s.model.turn,s.info.move, index, 3,3),
                                (s.model.board,s.model.turn,s.info.move, index, 4,3),
                                (s.model.board,s.model.turn,s.info.move, index, 5,3),
                                (s.model.board,s.model.turn,s.info.move, index, 6,3),
                                (s.model.board,s.model.turn,s.info.move, index, 7,3),
                                (s.model.board,s.model.turn,s.info.move, index, 8,3)
                            ])
        in 
            if (s.info.move == 4)
                then aux + 100
                else aux


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

getStringTurn : Model -> Turn -> String
getStringTurn m t = 
    if (m.ai==Nothing)
        then
            case t of
                Black -> "Black"
                White -> "White"
        else
            if (m.ai == (just t))
                then "CPU"
                else "Human"


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
analyzeState b t pilar index = containsL (List.map followPath   [(b,t,pilar,index,1,4),
                                                                 (b,t,pilar,index,2,4),
                                                                 (b,t,pilar,index,3,4),
                                                                 (b,t,pilar,index,4,4),
                                                                 (b,t,pilar,index,5,4),
                                                                 (b,t,pilar,index,6,4),
                                                                 (b,t,pilar,index,7,4),
                                                                 (b,t,pilar,index,8,4)]) True

followPath : (Board,Turn,Int,Int,Int,Int) -> Bool
followPath (b,t,pilar,index,path,line) = followPathR b t pilar index path line

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

center : Attribute
center = 
    style
        [
        ("margin","auto"),
        ("width", "70%")        
        ]

bigButton : Attribute
bigButton = 
    style
        [
        ("width","200px"),
        ("height","200px"),
        ("margin-top","150px")
        ]

mediumButton : Attribute
mediumButton = 
    style
        [
        ("width","400px"),
        ("height","50px")
        ]

smallButton : Attribute
smallButton = 
    style
        [
        ("backgroundColor","grey")
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
getViewStart address model = div [center] [button [onClick address (Start), bigButton ] [ text "Play!" ]]

getViewInGameHuman : Signal.Address Action -> Model -> Html
getViewInGameHuman address model = 
        div [center]
            [ 
            h1 [] [text ("Turn: " ++ getStringTurn model model.turn)],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 1 6)],
                h2 [] [text (getStringBoard model.board 1 5)],
                h2 [] [text (getStringBoard model.board 1 4)],
                h2 [] [text (getStringBoard model.board 1 3)],
                h2 [] [text (getStringBoard model.board 1 2)],
                h2 [] [text (getStringBoard model.board 1 1)],
                button [onClick address (Move 1),smallButton] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 2 6)],
                h2 [] [text (getStringBoard model.board 2 5)],
                h2 [] [text (getStringBoard model.board 2 4)],
                h2 [] [text (getStringBoard model.board 2 3)],
                h2 [] [text (getStringBoard model.board 2 2)],
                h2 [] [text (getStringBoard model.board 2 1)],
                button [onClick address (Move 2),smallButton] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 3 6)],
                h2 [] [text (getStringBoard model.board 3 5)],
                h2 [] [text (getStringBoard model.board 3 4)],
                h2 [] [text (getStringBoard model.board 3 3)],
                h2 [] [text (getStringBoard model.board 3 2)],
                h2 [] [text (getStringBoard model.board 3 1)],
                button [onClick address (Move 3),smallButton] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 4 6)],
                h2 [] [text (getStringBoard model.board 4 5)],
                h2 [] [text (getStringBoard model.board 4 4)],
                h2 [] [text (getStringBoard model.board 4 3)],
                h2 [] [text (getStringBoard model.board 4 2)],
                h2 [] [text (getStringBoard model.board 4 1)],
                button [onClick address (Move 4),smallButton] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 5 6)],
                h2 [] [text (getStringBoard model.board 5 5)],
                h2 [] [text (getStringBoard model.board 5 4)],
                h2 [] [text (getStringBoard model.board 5 3)],
                h2 [] [text (getStringBoard model.board 5 2)],
                h2 [] [text (getStringBoard model.board 5 1)],
                button [onClick address (Move 5),smallButton] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 6 6)],
                h2 [] [text (getStringBoard model.board 6 5)],
                h2 [] [text (getStringBoard model.board 6 4)],
                h2 [] [text (getStringBoard model.board 6 3)],
                h2 [] [text (getStringBoard model.board 6 2)],
                h2 [] [text (getStringBoard model.board 6 1)],
                button [onClick address (Move 6),smallButton] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 7 6)],
                h2 [] [text (getStringBoard model.board 7 5)],
                h2 [] [text (getStringBoard model.board 7 4)],
                h2 [] [text (getStringBoard model.board 7 3)],
                h2 [] [text (getStringBoard model.board 7 2)],
                h2 [] [text (getStringBoard model.board 7 1)],
                button [onClick address (Move 7),smallButton] [ text "-" ]
                ]
            ]

getViewInGameCPU : Signal.Address Action -> Model -> Html
getViewInGameCPU address model = 
    let info = ai model
    in
    -- TODO
        div [center]
            [ 
            h1 [] [text (getStringTurn model model.turn ++ " thinking...")],
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
                h2 [] [text (getStringBoard model.board 7 1)]],
            div [] [button [onClick address (Move info.bestMove),mediumButton] [ text "Move!" ]]]  

getViewWon : Signal.Address Action -> Model -> Html
getViewWon address model = 
        div [center]
            [ 
            div [] [h1 [] [text (getStringTurn model (updateTurn model.turn) ++ " Won!")]],
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
                h2 [] [text (getStringBoard model.board 7 1)]],
            div [] [button [onClick address (Replay),mediumButton] [ text "Replay" ]]]   


getViewTied : Signal.Address Action -> Model -> Html
getViewTied address model = 
        div [center]
            [ 
            div [] [h2 [] [text "Tied"]],
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
                h2 [] [text (getStringBoard model.board 7 1)]],
            div [] [button [onClick address (Replay)] [ text "Replay" ]]]   

-- main

main = StartApp.start {model = init, update = update, view = view}
