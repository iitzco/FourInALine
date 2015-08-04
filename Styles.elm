module Styles where

import Html.Attributes exposing (style,type',checked,src)
import Html exposing (..)

{-
Styles - Style attributes for Html
-}

inline : Attribute
inline =
  style
    [ ("display", "inline-block"),
      ("width","300px")
    ]


width : Int
width = 750

center : Attribute
center = 
    style
        [
        ("margin","0 auto"),
        ("width", toString width ++ "px")      
        ]

tableBorder : Attribute
tableBorder =
    style
        [
        ("border","1px solid gray"),
        ("background-color","gray")
        ]

marginUp : Attribute
marginUp =
    style
        [
        ("margin","10px")
        ]

