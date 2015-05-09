import Signal
import Text
import Graphics.Element
import Graphics.Input

checkedChnl : Signal.Channel Bool
checkedChnl = Signal.channel True

display : Bool -> Graphics.Element.Element
display checked = 
  Graphics.Element.flow Graphics.Element.right 
    [
        Graphics.Input.checkbox (Signal.send checkedChnl) checked
--      , (Text.asText checked)
    ]
    
main : Signal.Signal Graphics.Element.Element
main = Signal.map display (Signal.subscribe checkedChnl)
    