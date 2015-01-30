import Text (..)
--import Graphics.Element (..)
import Graphics.Element 

--import Signal(Signal, constant, map)
import Signal
--import Signal(map)

import Mouse


main : Signal.Signal Graphics.Element.Element
--main = fromString "hi world" (fails, as Text needs to become Element)
-- <| is Haskell $
-- NOTE: this is pretty much asText code
-- works
--main = leftAligned (fromString "hi world")
-- works
--main = leftAligned <| fromString "hi world"

--main = Signal.constant "hi world"
--works
--main = Signal.map asText (Signal.constant "hi world")



--works
main = Signal.map asText (clickCount)

clickCount : Signal Int
clickCount =
  Signal.foldp (\click count -> count + 1) 0 Mouse.clicks



