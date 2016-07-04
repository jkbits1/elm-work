-- NOTE: code comes from Signal.send package example
-- have changed fn names to show lifting fns (not sure if it is clearer)

--import Signal (channel)
--import Signal
--import Html
import Html exposing (..)
import Html.App as HtmlApp
import Html.Attributes exposing (class)
import Html.Events
--import Graphics.Input.Field as Field
--import Graphics.Element exposing (..)

-- from Elm Architecture tutorial, but suitable here too
type alias Model = Int

--type Update = NoOp | Add Int | Remove Int
type Msg = NoOp | Add Int | Remove Int

--updatesChnl : Signal.Mailbox Update
--updatesChnl = Signal.mailbox NoOp

-- created by view, returns Html and sends Updates to updatesChnl
addButton : Html Msg
addButton = Html.button
--  [ Html.Events.onClick (Signal.send updates (Add 1))]
--  [ Html.Events.onClick updatesChnl.address (Add 1)]
  [ Html.Events.onClick (Add 1)]
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

init = (1, Cmd.none)
  
-- converts Signal Model to Signal Html, using non-signal view
--main : Signal Html
--main = Signal.map (view updatesChnl.address) updateModelLift
--main = viewLift
main = HtmlApp.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

subscriptions : Int -> Sub Msg
subscriptions model = Sub.none

--viewLift : Signal Html
--viewLift = Signal.map (view updatesChnl.address) updateModel

-- used by main, as a non-signal function, to convert a Model to Html
view : Model -> Html Msg
--view : Signal.Address Update -> Model -> Html
view model =
--view address model =
  div [class "container"]
  [
    addButton,    
    div [] [ text (toString model) ]
  ]

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
--updateModelLift : Signal Model
--updateModelLift = Signal.foldp updateModel 0 updatesChnl.signal

-- converts Update to new Model
--updateModel : Update -> Model -> Model
updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel update model =
  case update of
    NoOp        -> (model, Cmd.none)
    Add val     -> (model + 1, Cmd.none)
    Remove val  -> (model - 1, Cmd.none)
