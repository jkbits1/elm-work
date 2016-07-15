import Html exposing (..)
import Html.App as HtmlApp
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Text exposing (color)
import Color exposing (..)

import String
import List exposing (..)
-- import Graphics.Element (Element)
-- import Text (asText)

factorial : Int -> Int
factorial n =
  if n <= 1
    then 1
    else n * factorial (n-1)
    
leng xs = 
        case xs of
            [] -> 0
            x :: xs -> 1 + leng xs

-- main : Element
main =
  text <| toString (factorial 5)
