import Signal
import Text
import Graphics.Element
import Graphics.Input

checkedChnl : Signal.Mailbox Bool
checkedChnl = Signal.mailbox True

display : Bool -> Graphics.Element.Element
display checked = 
  Graphics.Element.flow Graphics.Element.right 
    [
        Graphics.Input.checkbox (Signal.message checkedChnl.address) checked
--      , (Text.asText checked)
    ]
    
main : Signal.Signal Graphics.Element.Element
main = Signal.map display checkedChnl.signal
    