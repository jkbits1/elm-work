import Html exposing (..)
-- import Html.App as HtmlApp
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
-- import Text exposing (color)
-- import Color exposing (..)

import String exposing (..)
import List exposing (..)

type alias Model = String

type Msg =
      Change String 

main = Html.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view s =
  div [
    class "container"
  ] [
    h2 [] [text "Elm Basic"]
    
  , input [ onInput Change ] []
  
  , br [] []
  , br [] []
    
  , text  s
  , br [] []
  , text  <| toUpper s
  , br [] []
  , text  <| toLower s
  ]

-- converts Update to new Model
updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel update s = 
    case update of
      Change s1       ->    (s1, Cmd.none)


initialModelState = "hello, Front End London"

init = (initialModelState, Cmd.none )


-- converts Signal Model to Signal Html, using non-signal view
--main : Signal Html
--main = viewLift

--viewLift : Signal Html
--viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
--updateModelLift : Signal Model
--updateModelLift = Signal.foldp
--                    updateModel
--                    initialModelState
--                    updatesChnl.signal

