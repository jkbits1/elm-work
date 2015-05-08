import Signal
--import Signal (map)
import Graphics.Element
import Graphics.Input

--NOTE: if Text is not imported, code crashes
import Text

checkedChnl : Signal.Channel Bool
checkedChnl = Signal.channel False

boxes : Bool -> Graphics.Element.Element
boxes checked =
  let box = (Graphics.Input.checkbox (Signal.send checkedChnl) checked)
  in
    Graphics.Element.flow Graphics.Element.right 
      [ box, box, box ]
--  let box = 
--  in
--    Graphics.Element.flow Graphics.Element.right 
--      [ 
--        Graphics.Input.checkbox (Signal.send check) checked
----        , box, box 
--      ]
    
main : Signal.Signal Graphics.Element.Element
main = 
--  boxes <~ Signal.subscribe check
  Signal.map boxes (Signal.subscribe checkedChnl)

