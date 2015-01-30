import Signal
import Text(asText)
import Mouse
import Graphics.Element(Element)

main : Signal Element
main  =
  Signal.map asText Mouse.isDown