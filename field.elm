import Signal
import Graphics.Input.Field
import Graphics.Element
import Text
import String

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
  ]
  
main : Signal Graphics.Element.Element
main = Signal.map scene (Signal.subscribe contentChnl)

rev : String -> String
rev str = String.reverse str