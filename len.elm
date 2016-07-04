import List exposing ((::))
import Html exposing (text)
--import Graphics.Element(Element)

length : List a -> Int
length list =
  case list of
    [] -> 0

    first :: rest ->
      1 + length rest

--main : Element
main = text <| toString (length [1..9])
