{-
Contains Model, Update, View
-}

--Elm Core and Packages
import Html exposing (..)
import Html.Attributes exposing (style,type',checked,src)
import Html.Events exposing (onClick,targetChecked,on)
import StartApp
import Svg exposing (svg,circle)
import Svg.Attributes exposing (cx,fill,cy,r,stroke)
import Debug

--Files .elm
import API exposing (..)
import Styles exposing (..)


{-

Artificial Intelligence - Minimax algorithm.
--------------------------------------------

-}

type alias Info =
    {
    move : Int,
    bestMove : Int,
    heuristic : Int
    }

type alias State = 
    {
    model : Model,
    info : Info
    }

type GTree a = Node a (List (GTree a))

initInfo : Int -> Int -> Int -> Info
initInfo heuristicParam moveParam bestMoveParam = { move = moveParam , heuristic = heuristicParam, bestMove = bestMoveParam }



ai : Model -> Info
ai model = minimax (generateState 0 model) model.prof

generateState : Int -> Model -> State
generateState moveParam modelParam = 
                {
                model = modelParam,
                info = initInfo 0 moveParam 0
                }

generateTree : Int -> State -> GTree State
generateTree p s = if (p/=s.model.prof && (p == 0 || s.model.status/=InGame)) then (Node s []) else (Node s (List.map (generateTree (p-1)) (getStates s)))


getStates : State -> List State
getStates s = List.map (\pilar -> generateState pilar (makeMove s.model pilar)) (List.filter (\pilar -> not (isFull (getL s.model.board pilar))) [1,2,3,4,5,6,7]) 

minimax : State -> Int -> Info
minimax s p = (getHead (minimaxR p (generateTree p s))).info

minimaxR : Int -> GTree State -> GTree State
minimaxR p (Node s l) = 
    let i = 
        if (p/=s.model.prof && (p == 0 ||  s.model.status /= InGame))
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

{-

Heuristics - Quantitative value of a board for Minimax Algrithm.
----------------------------------------------------------------

-}


getHeuristic : State -> Int
getHeuristic s = 
    let h = getH1 s
    in
        if (s.model.turn /= unJust s.model.ai)
            then h
            else (-1)*h


--only checks wins
getH1 : State -> Int 
getH1 s = 
    if (s.model.status == Won)
        then inf
        else Debug.watch "Hs" ((getH2 s) + (getH3 s))


getH2 : State -> Int
getH2 s = List.foldr (\pilar count -> count + (getPilarValue (pilar) * (countElem (getL s.model.board pilar) (just (updateTurn s.model.turn))-countElem (getL s.model.board pilar) (just s.model.turn)))) 0 [1,2,3,4,5,6,7]

getH3 : State -> Int
getH3 s = List.foldr (\pilar count -> count + (getPilarLines pilar s.model 3)) 0 [1,2,3,4,5,6,7]

getPilarLines : Int -> Model -> Int -> Int
getPilarLines pilar model line = List.foldr (\index count -> if (getM model.board pilar index == just (updateTurn model.turn)) then count + countLines model pilar index line else count) 0 [1,2,3,4,5,6]

getPilarValue : Int -> Int
getPilarValue p =
    case p of
        1 -> 10
        2 -> 20
        3 -> 30
        4 -> 40
        5 -> 30
        6 -> 20
        7 -> 10

countLines : Model -> Int -> Int -> Int -> Int
countLines model pilar index c = List.foldr (\b c -> if b then c + 10 else c) 0 (List.map followPath 
                            [
                                (model.board,model.turn,pilar, index, 1,c),
                                (model.board,model.turn,pilar, index, 2,c),
                                (model.board,model.turn,pilar, index, 3,c),
                                (model.board,model.turn,pilar, index, 4,c),
                                (model.board,model.turn,pilar, index, 5,c),
                                (model.board,model.turn,pilar, index, 6,c),
                                (model.board,model.turn,pilar, index, 7,c),
                                (model.board,model.turn,pilar, index, 8,c)
                            ])


{-

Model - Representation of the game. 
-----------------------------------

-}

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
    ai : Maybe Coin,
    prof : Int
    }

init : Model
init = { board = getEmptyBoard , turn = Black , status = Starting, coins = 0, ai = Nothing, prof = 3}


--For the board, each List of the Matrix is one pilar of the board.
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
                Black -> "Red"
                White -> "Blue"
        else
            if (m.ai == (just t))
                then "CPU"
                else "Human"


{-

Update - Represents all interactions (actions) with the model .
--------------------------------------------------------------

-}

type Action = Move Int | Start | Replay | Prof Int | AI (Maybe Turn) | MoveCPU

update : Action -> Model -> Model
update action model = 
    case action of
        Start -> {model | status <- InGame}
        Prof p -> {model | prof <- p}
        AI t -> {model | ai <- t}
        Replay -> init
        Move pilar -> makeMove model pilar
        MoveCPU -> makeMove model (ai model).bestMove

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


{-

View - Html depending on Model's status.
----------------------------------------

-}

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
        _ -> getViewWonTied address model

     

getImageLogo : Html
getImageLogo = 
    div [style [("margin","0 auto"),("width","500px")]]
    [
        img [src "img/logo.png",Html.Attributes.width (500),marginUp] []
    ]

getModeSelect : Signal.Address Action -> Model -> Html
getModeSelect address model = 
        div [style [("margin-left", "100px")]] [
            h3 [] [text "Select Mode: "],
            input [ type' "radio", checked (model.ai == Nothing) , on "change" targetChecked (\_ -> Signal.message address (AI Nothing))] []  , text " 2 Players" , br [] [] ,
            input [ type' "radio", checked (model.ai == Just Black), on "change" targetChecked (\_ -> Signal.message address (AI (Just Black)))] []  , text " vs CPU - CPU moves first" , br [] [] ,
            input [ type' "radio", checked (model.ai == Just White), on "change" targetChecked (\_ -> Signal.message address (AI (Just White)))] []  , text " vs CPU - Human moves first" , br [] []]

getLevelSelect : Signal.Address Action -> Model -> Html
getLevelSelect address model =
            div [style [("margin-left", "100px")]] [
                div [inline] [
                    h3 [] [text "Select Mode: "],
                    input [ type' "radio", checked (model.ai == Nothing) , on "change" targetChecked (\_ -> Signal.message address (AI Nothing))] []  , text " 2 Players" , br [] [] ,
                    input [ type' "radio", checked (model.ai == Just Black), on "change" targetChecked (\_ -> Signal.message address (AI (Just Black)))] []  , text " vs CPU - CPU moves first" , br [] [] ,
                    input [ type' "radio", checked (model.ai == Just White), on "change" targetChecked (\_ -> Signal.message address (AI (Just White)))] []  , text " vs CPU - Human moves first" , br [] [] ],
                div [inline] [
                    h3 [] [text "Select Level:"],
                    input [ type' "radio", checked (model.prof == 2) , on "change" targetChecked (\_ -> Signal.message address (Prof 2))] []  , text " Easy" , br [] []  ,
                    input [ type' "radio", checked (model.prof == 3), on "change" targetChecked (\_ -> Signal.message address (Prof 3))] []  , text " Medium" , br [] []  ,
                    input [ type' "radio", checked (model.prof == 4), on "change" targetChecked (\_ -> Signal.message address (Prof 4))] []  , text " Hard" , br [] []  ]]
getPlayButton : Signal.Address Action -> Html
getPlayButton address = 
    div [style [("margin", "0 auto"),("width","250px")]] 
    [
        img [src "img/playbutton.png",Html.Attributes.width 250,onClick address Start,style [("margin-top","20px")]] []
    ]


getViewStart : Signal.Address Action -> Model -> Html
getViewStart address model = 
        if (model.ai == Nothing)
            then
                
                div [center] 
                [   
                    getImageLogo,
                    getModeSelect address model,
                    getPlayButton address
                ]  
            else
                div [center]
                [ 
                    getImageLogo,
                    getLevelSelect address model,
                    getPlayButton address
                ]

cAttributes : String
cAttributes = toString 27

svgAttributes : String
svgAttributes = toString 54

rAttributes : String
rAttributes = toString 26

getFigure : Maybe Turn -> Svg.Svg
getFigure t =
    case  t of
        Nothing -> Svg.circle [cx cAttributes,cy cAttributes,r rAttributes, fill "White"] []
        Just Black -> Svg.circle [cx cAttributes,cy cAttributes,r rAttributes, fill "Red", stroke "Black"] []
        Just White -> Svg.circle [cx cAttributes,cy cAttributes,r rAttributes, fill "Blue", stroke "Black"] []

getSvg : Board -> Int -> Int -> Html
getSvg b r c = Svg.svg [Svg.Attributes.width svgAttributes,Svg.Attributes.height svgAttributes] [getFigure (getM b r c)]


getViewBoard : Signal.Address Action -> Model -> Bool -> Html
getViewBoard address model click = 
    if (click)
        then
            table [style [("margin","0 auto")]] [
                tr [tableBorder] [
                    td [tableBorder,onClick address (Move 1)] [getSvg model.board 1 6],
                    td [tableBorder,onClick address (Move 2)] [getSvg model.board 2 6],
                    td [tableBorder,onClick address (Move 3)] [getSvg model.board 3 6],
                    td [tableBorder,onClick address (Move 4)] [getSvg model.board 4 6],
                    td [tableBorder,onClick address (Move 5)] [getSvg model.board 5 6],
                    td [tableBorder,onClick address (Move 6)] [getSvg model.board 6 6],
                    td [tableBorder,onClick address (Move 7)] [getSvg model.board 7 6]],
                tr [tableBorder] [
                    td [tableBorder,onClick address (Move 1)] [getSvg model.board 1 5],
                    td [tableBorder,onClick address (Move 2)] [getSvg model.board 2 5],
                    td [tableBorder,onClick address (Move 3)] [getSvg model.board 3 5],
                    td [tableBorder,onClick address (Move 4)] [getSvg model.board 4 5],
                    td [tableBorder,onClick address (Move 5)] [getSvg model.board 5 5],
                    td [tableBorder,onClick address (Move 6)] [getSvg model.board 6 5],
                    td [tableBorder,onClick address (Move 7)] [getSvg model.board 7 5]],
                tr [tableBorder] [
                    td [tableBorder,onClick address (Move 1)] [getSvg model.board 1 4],
                    td [tableBorder,onClick address (Move 2)] [getSvg model.board 2 4],
                    td [tableBorder,onClick address (Move 3)] [getSvg model.board 3 4],
                    td [tableBorder,onClick address (Move 4)] [getSvg model.board 4 4],
                    td [tableBorder,onClick address (Move 5)] [getSvg model.board 5 4],
                    td [tableBorder,onClick address (Move 6)] [getSvg model.board 6 4],
                    td [tableBorder,onClick address (Move 7)] [getSvg model.board 7 4]],
                tr [tableBorder] [
                    td [tableBorder,onClick address (Move 1)] [getSvg model.board 1 3],
                    td [tableBorder,onClick address (Move 2)] [getSvg model.board 2 3],
                    td [tableBorder,onClick address (Move 3)] [getSvg model.board 3 3],
                    td [tableBorder,onClick address (Move 4)] [getSvg model.board 4 3],
                    td [tableBorder,onClick address (Move 5)] [getSvg model.board 5 3],
                    td [tableBorder,onClick address (Move 6)] [getSvg model.board 6 3],
                    td [tableBorder,onClick address (Move 7)] [getSvg model.board 7 3]],
                tr [tableBorder] [
                    td [tableBorder,onClick address (Move 1)] [getSvg model.board 1 2],
                    td [tableBorder,onClick address (Move 2)] [getSvg model.board 2 2],
                    td [tableBorder,onClick address (Move 3)] [getSvg model.board 3 2],
                    td [tableBorder,onClick address (Move 4)] [getSvg model.board 4 2],
                    td [tableBorder,onClick address (Move 5)] [getSvg model.board 5 2],
                    td [tableBorder,onClick address (Move 6)] [getSvg model.board 6 2],
                    td [tableBorder,onClick address (Move 7)] [getSvg model.board 7 2]],
                tr [tableBorder] [
                    td [tableBorder,onClick address (Move 1)] [getSvg model.board 1 1],
                    td [tableBorder,onClick address (Move 2)] [getSvg model.board 2 1],
                    td [tableBorder,onClick address (Move 3)] [getSvg model.board 3 1],
                    td [tableBorder,onClick address (Move 4)] [getSvg model.board 4 1],
                    td [tableBorder,onClick address (Move 5)] [getSvg model.board 5 1],
                    td [tableBorder,onClick address (Move 6)] [getSvg model.board 6 1],
                    td [tableBorder,onClick address (Move 7)] [getSvg model.board 7 1]]]
            else
                table [style [("margin","0 auto")]] [
                tr [tableBorder] [
                    td [tableBorder] [getSvg model.board 1 6],
                    td [tableBorder] [getSvg model.board 2 6],
                    td [tableBorder] [getSvg model.board 3 6],
                    td [tableBorder] [getSvg model.board 4 6],
                    td [tableBorder] [getSvg model.board 5 6],
                    td [tableBorder] [getSvg model.board 6 6],
                    td [tableBorder] [getSvg model.board 7 6]],
                tr [tableBorder] [
                    td [tableBorder] [getSvg model.board 1 5],
                    td [tableBorder] [getSvg model.board 2 5],
                    td [tableBorder] [getSvg model.board 3 5],
                    td [tableBorder] [getSvg model.board 4 5],
                    td [tableBorder] [getSvg model.board 5 5],
                    td [tableBorder] [getSvg model.board 6 5],
                    td [tableBorder] [getSvg model.board 7 5]],
                tr [tableBorder] [
                    td [tableBorder] [getSvg model.board 1 4],
                    td [tableBorder] [getSvg model.board 2 4],
                    td [tableBorder] [getSvg model.board 3 4],
                    td [tableBorder] [getSvg model.board 4 4],
                    td [tableBorder] [getSvg model.board 5 4],
                    td [tableBorder] [getSvg model.board 6 4],
                    td [tableBorder] [getSvg model.board 7 4]],
                tr [tableBorder] [
                    td [tableBorder] [getSvg model.board 1 3],
                    td [tableBorder] [getSvg model.board 2 3],
                    td [tableBorder] [getSvg model.board 3 3],
                    td [tableBorder] [getSvg model.board 4 3],
                    td [tableBorder] [getSvg model.board 5 3],
                    td [tableBorder] [getSvg model.board 6 3],
                    td [tableBorder] [getSvg model.board 7 3]],
                tr [tableBorder] [
                    td [tableBorder] [getSvg model.board 1 2],
                    td [tableBorder] [getSvg model.board 2 2],
                    td [tableBorder] [getSvg model.board 3 2],
                    td [tableBorder] [getSvg model.board 4 2],
                    td [tableBorder] [getSvg model.board 5 2],
                    td [tableBorder] [getSvg model.board 6 2],
                    td [tableBorder] [getSvg model.board 7 2]],
                tr [tableBorder] [
                    td [tableBorder] [getSvg model.board 1 1],
                    td [tableBorder] [getSvg model.board 2 1],
                    td [tableBorder] [getSvg model.board 3 1],
                    td [tableBorder] [getSvg model.board 4 1],
                    td [tableBorder] [getSvg model.board 5 1],
                    td [tableBorder] [getSvg model.board 6 1],
                    td [tableBorder] [getSvg model.board 7 1]]]


getViewInGameHuman : Signal.Address Action -> Model -> Html
getViewInGameHuman address model = 
        div [center]
            [ 
            getImageLogo,
            h3 [Html.Attributes.align "center",style [("margin","0")]] [text ("Turn: " ++ getStringTurn model model.turn)],
            getViewBoard address model True,
            div [style [("margin", "0 auto"),("width","250px")]] 
                [
                    h4 [Html.Attributes.align "center"] [text "Click a column to make move..."]
                ]
            ]

getViewInGameCPU : Signal.Address Action -> Model -> Html
getViewInGameCPU address model = 
        div [center] 
        [ 
                getImageLogo,
                h3 [Html.Attributes.align "center",style [("margin","0")]] [text ("Turn: " ++ getStringTurn model model.turn)],
                getViewBoard address model False,
                div [style [("margin", "0 auto"),("width","150px")]] 
                [
                    img [src "img/next.png",Html.Attributes.width 150,onClick address MoveCPU,style [("margin-top","20px")]] [],
                    h4 [Html.Attributes.align "center",style [("margin-top","0")]] [text "Click ONCE and wait..."]
                ]
         ]
            

getStringStatus : Model -> String
getStringStatus m = 
    case m.status of
        Won -> (getStringTurn m (updateTurn m.turn)) ++ " Won!"
        Tied -> "Tied"
        _ -> " "

getViewWonTied : Signal.Address Action -> Model -> Html
getViewWonTied address model = 
        let s = getStringStatus model
        in
            div [center]
            [ 
                getImageLogo,
                div [] [h3 [Html.Attributes.align "center",style [("margin","0")]] [text s]],
                getViewBoard address model False,
                div [style [("margin", "0 auto"),("width","100px")]] 
                [
                    img [src "img/replay.png",Html.Attributes.width 100,onClick address Replay,style [("margin-top","20px")]] []
                ]
                
            ] 

-- Main

main = StartApp.start {model = init, update = update, view = view}
