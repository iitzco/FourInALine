{-
Contains Model, Update, View
-}

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp

--API
bottom = bottom

get : List a -> Int -> a
get l n = 
    case l of
        [] -> bottom
        x::xs -> if (n == 1) then x else get xs (n-1) 

put : List a -> a -> Int -> List a
put (x::xs) a n = if (n == 1) then (a :: xs) else x :: put xs a (n-1)


-- MODEL

type Coin = Black | White

type alias Turn = Coin

type alias Board = List (List (Maybe Coin)) 

type alias Model = 
    {
    board : Board,
    turn : Turn
    }

init : Model
init = { board = getEmptyBoard , turn = Black}


{-6x7 board:
    xxxxxxx
    xxxxxxx
    xxxxxxx
    xxxxxxx
    xxxxxxx
    xxxxxxx
-}

getEmptyBoard : Board
getEmptyBoard = List.repeat 7 (List.repeat 6 Nothing)


getStringBoard : Board -> Int -> Int -> String
getStringBoard b column row =
    formString (get (get b column) row)

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

type Action = Move Int

update : Action -> Model -> Model
update (Move n) model = 
    if (isFull (get model.board n)) then
        model
    else
        {
        model   | turn <- updateTurn model.turn,
                  board <- updateBoard model.board model.turn n
        }

updateTurn : Turn -> Turn
updateTurn t = if (t==Black) then White else Black

updateBoard : Board -> Turn -> Int -> Board
updateBoard b t n = put b (putCoin (get b n) t) n



putCoin : List (Maybe Coin) -> Turn -> List (Maybe Coin)
putCoin (x::xs) t = if (x == Nothing) then (Just t) :: xs else x :: putCoin xs t

toCoin : Turn -> Coin
toCoin = \t -> t

isFull : List (Maybe Coin) -> Bool
isFull l = 
    case l of
        [] -> True
        x::xs -> if (x == Nothing) then False else isFull xs


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

-- main

main = StartApp.start {model = init, update = update, view = view}




