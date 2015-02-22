import Http
import Signal (Signal, constant, map)
import Text (asText)
import Graphics.Element(Element)

addr : a -> Signal a
addr = 
  \a -> constant a

main : Signal Element
main = 
  map asText (Http.sendGet (addr "http://localhost:3030/foldersDb"))