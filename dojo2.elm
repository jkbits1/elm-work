--import Graphics.Element (..)
--import Graphics.Collage (..)

--import Color
--import Graphics.Element
--import Markdown

--import Html (..)

import Debug
import Graphics.Element as Element
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Regex
import Signal (..)
import String
import Window



--main = render <~ state
main : Element.Element
main = render 

render : Element.Element
render = 
  toElement 200 200 <| 
  
    input [
--  inputStyle
      ]
      []
  
  