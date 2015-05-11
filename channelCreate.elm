-- NOTE: code comes from Signal.send package example

--import Signal (channel)
import Signal 
--import Html
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events
import Graphics.Input.Field as Field
import Graphics.Element exposing (..)

-- from Elm Architecture tutorial, but suitable here too
type alias Model = Int

type Update = NoOp | Add Int | Remove Int

updatesChnl : Signal.Mailbox Update
updatesChnl = Signal.mailbox NoOp

addButton : Html
addButton = Html.button
--  [ Html.Events.onClick (Signal.send updates (Add 1))]
  [ Html.Events.onClick updatesChnl.address (Add 1)]
  [ Html.text "Add 1" ]

-- this bit, from Signal package example doesn't work
--main : Signal Element
--main = Signal.map 
--  (view updates)
--  (foldp step initialState (subscribe updates))

-- from text field example

--view : Field.Content -> Element
--view fieldContent =
--  flow down
--  [ addButton ]
  
--main = div [class "container"]
--  [
--    addButton
--  ]

main : Signal Html
main = Signal.map (view updatesChnl.address) model

view : Signal.Address Update -> Model -> Html
view address model = div [class "container"]
  [
    addButton,    
    div [] [ text (toString model) ]
  ]

model : Signal Model
model = Signal.foldp updateModel 0 updatesChnl.signal 

updateModel : Update -> Model -> Model
updateModel update model =
  case update of
    NoOp        -> model 
    Add val     -> model + 1    
    Remove val  -> model - 1
    