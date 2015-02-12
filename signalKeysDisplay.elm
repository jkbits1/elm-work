

import Signal 
import Text (asText, plainText)
import Keyboard 
import Graphics.Element


main : Signal Graphics.Element.Element
main = 
  Signal.map display Keyboard.keysDown 

display : List Keyboard.KeyCode -> Graphics.Element.Element
display keys =
  Graphics.Element.flow Graphics.Element.right [
    plainText "keys are : ",
    asText keys
  ]
  