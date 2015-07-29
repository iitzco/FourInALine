{-
Contains Model, Update, View
-}

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp
import Debug

--API

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

-- MODEL

type Coin = Black | White

type State = Starting | InGame | Won | Tied

type alias Turn = Coin

type alias Board = Matrix (Maybe Coin)

type alias Model = 
    {
    board : Board,
    turn : Turn,
    state : State
    }

init : Model
init = { board = getEmptyBoard , turn = Black , state = Starting }


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
        Start -> {model | state <- InGame}
        Replay -> init
        Move pilar ->
            let listPilar = getL model.board pilar
            in
                if (isFull listPilar) then
                    model
                else
                    let index = getLIndex listPilar Nothing
                    in 
                        {
                        model   | board <- updateBoard model.board model.turn pilar index,
                                  turn <- updateTurn model.turn,
                                  state <- 
                                    if (analyzeState model.board model.turn pilar index) 
                                        then Won 
                                        else
                                           -- if (tiedGame model.board) then Tied else InGame
                                           InGame
                        }

updateTurn : Turn -> Turn
updateTurn t = if (t==Black) then White else Black

updateBoard : Board -> Turn -> Int -> Int -> Board
updateBoard b t pilar index = putM b pilar index (Just t)

isFull : List (Maybe Coin) -> Bool
isFull l = not (containsL l Nothing)

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
                        if (remain2 == 1)
                            then True
                        else
                            followPathR b t pilar2 index2 path remain2

tiedGame : Board -> Bool
tiedGame b = not (containsL (List.map (isFull) [getL b 1,getL b 2,getL b 3,getL b 4,getL b 5,getL b 6,getL b 7]) False)



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

view : Signal.Address Action -> Model -> Html
view adress model = 
    case model.state of
        Starting ->
            div [] [button [onClick adress (Start)] [ text "Play!" ]]
        InGame ->
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
                button [onClick adress (Move 1)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 2 6)],
                h2 [] [text (getStringBoard model.board 2 5)],
                h2 [] [text (getStringBoard model.board 2 4)],
                h2 [] [text (getStringBoard model.board 2 3)],
                h2 [] [text (getStringBoard model.board 2 2)],
                h2 [] [text (getStringBoard model.board 2 1)],
                button [onClick adress (Move 2)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 3 6)],
                h2 [] [text (getStringBoard model.board 3 5)],
                h2 [] [text (getStringBoard model.board 3 4)],
                h2 [] [text (getStringBoard model.board 3 3)],
                h2 [] [text (getStringBoard model.board 3 2)],
                h2 [] [text (getStringBoard model.board 3 1)],
                button [onClick adress (Move 3)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 4 6)],
                h2 [] [text (getStringBoard model.board 4 5)],
                h2 [] [text (getStringBoard model.board 4 4)],
                h2 [] [text (getStringBoard model.board 4 3)],
                h2 [] [text (getStringBoard model.board 4 2)],
                h2 [] [text (getStringBoard model.board 4 1)],
                button [onClick adress (Move 4)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 5 6)],
                h2 [] [text (getStringBoard model.board 5 5)],
                h2 [] [text (getStringBoard model.board 5 4)],
                h2 [] [text (getStringBoard model.board 5 3)],
                h2 [] [text (getStringBoard model.board 5 2)],
                h2 [] [text (getStringBoard model.board 5 1)],
                button [onClick adress (Move 5)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 6 6)],
                h2 [] [text (getStringBoard model.board 6 5)],
                h2 [] [text (getStringBoard model.board 6 4)],
                h2 [] [text (getStringBoard model.board 6 3)],
                h2 [] [text (getStringBoard model.board 6 2)],
                h2 [] [text (getStringBoard model.board 6 1)],
                button [onClick adress (Move 6)] [ text "-" ]
                ],
            div [inline]
                [
                h2 [] [text (getStringBoard model.board 7 6)],
                h2 [] [text (getStringBoard model.board 7 5)],
                h2 [] [text (getStringBoard model.board 7 4)],
                h2 [] [text (getStringBoard model.board 7 3)],
                h2 [] [text (getStringBoard model.board 7 2)],
                h2 [] [text (getStringBoard model.board 7 1)],
                button [onClick adress (Move 7)] [ text "-" ]
                ]
            ]
        Won ->
            div []
            [ 
            div [] [h2 [] [text "Won"]],
            button [onClick adress (Replay)] [ text "Replay" ],
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
            
        Tied ->
            div []
            [
            div [] [h2 [] [text "Tied"]],
            button [onClick adress (Replay)] [ text "Replay" ],
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
