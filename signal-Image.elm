import Signal
import Graphics.Element(Element, image)
import Mouse

main : Signal Element
main =
  Signal.map makeImage getMouseValue
  
makeImage : Int -> Element  
makeImage = 
  \n -> image n n "/batmobile.jpg"
  


--getMouseValue : Signal 
getMouseValue : Signal Int
getMouseValue =
  Signal.map (\(x, y) -> max x y) Mouse.position 
  