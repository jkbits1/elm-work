

import Signal 
import Text (asText)
import Keyboard 
import Graphics.Element


main : Signal Graphics.Element.Element
main = 
  Signal.map asText Keyboard.keysDown 