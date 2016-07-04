import Signal
import Graphics.Input.Field
import Graphics.Element
import Text
import String
import Http

contentChnl : Signal.Channel Graphics.Input.Field.Content
contentChnl = Signal.channel Graphics.Input.Field.noContent

scene : Graphics.Input.Field.Content -> Graphics.Element.Element
scene fieldContent = 
  Graphics.Element.flow Graphics.Element.down
  [
      Graphics.Input.Field.field 
        Graphics.Input.Field.defaultStyle
        (Signal.send contentChnl)
        "Text"
        fieldContent
      , Text.asText <| rev fieldContent.string
--      , Text.asText <| Http.sendGet <| addr fieldContent.string
  ]
  
main : Signal Graphics.Element.Element
main = Signal.map scene (Signal.subscribe contentChnl)

rev : String -> String
rev str = String.reverse str

--addr : a -> Signal a
addr : String -> Signal String
--addr = \a -> Signal.constant a
addr = \a -> Signal.constant "http://localhost:3030/foldersDb"

