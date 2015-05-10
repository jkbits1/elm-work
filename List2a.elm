import List exposing (head, drop)
--import Text exposing (...)
import Graphics.Element exposing (show)

-- main : Signal Element
main : Graphics.Element.Element
--main = asText (head (drop 1 [1,2,3]))
main = show (head (drop 1 [1,2,3]))
