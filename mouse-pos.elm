import Graphics.Element (..)
-- import Signal (Signal, map)
import Signal (..)
import Mouse
-- import Text (asText)
import Text (..)


main : Signal Element
-- main = asText (head (drop 1 [1,2,3]))
-- main = plainText (head (drop 1 [1,2,3]))
main = map asText Mouse.position
