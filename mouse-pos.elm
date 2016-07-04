--import Graphics.Element (..)
-- import Signal (Signal, map)
--import Signal (..)
import Mouse exposing (position)
--import Text (asText, plainText)
--import Text (..)
import Html exposing (text)


--main : Signal Element
-- main = asText (head (drop 1 [1,2,3]))
-- main = plainText (head (drop 1 [1,2,3]))
--main = List.map (\pos -> text <| toString pos) position
main = text <| toString position
