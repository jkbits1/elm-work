import List
--import Text (..)
--import Graphics.Element(Element)
import Html exposing (text)

--main : Element
main = text <| toString (List.drop 2 [1,2,3])
