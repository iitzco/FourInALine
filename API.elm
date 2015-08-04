module API where

{-
API - This file includes extra functions to manage Lists and also new type Matrix.
-}

just : a -> Maybe a
just a = (Just a)

unJust : Maybe a -> a
unJust (Just a ) = a

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

countElem : List a -> a -> Int
countElem l elem = List.foldr (\x c-> if (x == elem) then c+1 else c) 0 l


getM : Matrix a -> Int -> Int -> a
getM m r c = getL (getL m r) c

putM : Matrix a -> Int -> Int -> a -> Matrix a
putM m r c elem = 
    case m of
        [] -> bottom
        x::xs -> if (r == 1) then (putL x elem c) :: xs else x :: putM xs (r-1) c elem 


emptyMatrix : Int -> Int -> a -> (Matrix a)
emptyMatrix row col elem = List.repeat row (List.repeat col elem)



inf : Int
inf = 1000
