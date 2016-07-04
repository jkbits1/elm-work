--import Text(..)
--import Graphics.Element(Element)
import Html exposing (text)

hypot : Float -> Float -> Float
hypot a b = 
  sqrt((a^2) + (b^2))

--main : Element
main = text <| toString [hypot 3 4, hypot 5 12, hypot 8 15]

