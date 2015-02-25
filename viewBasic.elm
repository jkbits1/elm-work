import Graphics.Element (flow, down, Element, container, middle)
import Graphics.Input.Field 
import Signal
import Text (fromString, asText, leftAligned)

main : Signal Element
main = Signal.map view (Signal.subscribe contentChnl)

contentChnl : Signal.Channel Graphics.Input.Field.Content
contentChnl = Signal.channel Graphics.Input.Field.noContent

view : Graphics.Input.Field.Content -> Element
view fieldContent =
  flow down 
    [
        leftAligned <| fromString "type in either field"
--      , myField identity "F" fieldContent
      , myField "F" fieldContent
--      , myField identity "B" fieldContent
      , myField "B" fieldContent
    ]

myField : String -> Graphics.Input.Field.Content -> Element
myField placeHolder fieldContent =
  Graphics.Input.Field.field Graphics.Input.Field.defaultStyle (Signal.send contentChnl) placeHolder fieldContent |> container 300 50 middle

