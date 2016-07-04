-- NOTE: code comes from Signal.send package example
-- have changed fn names to show lifting fns (not sure if it is clearer)

--import Signal (channel)
--import Signal
--import Html
--import Html exposing (Html, div, text, input, placeholder)
--import Html.Attributes exposing (class)
--import Html.Events

import Html exposing (..)
import Html.App as HtmlApp
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

--import Graphics.Input.Field as Field
--import Graphics.Element exposing (..)

import String
import List exposing (..)

type alias Model = (Int, String, String, String, String,
                         String, String, String)

type Msg =
      NoOp | Add Int | Remove Int |
      UpdateField String | Circle2Field String | Circle3Field String | Circle4Field String

type CircleChange = String

--updatesChnl : Signal.Mailbox Update
--updatesChnl = Signal.mailbox NoOp


-- created by view, returns Html and sends Updates to updatesChnl
addButton : Html Msg
addButton = Html.button
  [ Html.Events.onClick (Add 1)]
  [ Html.text "Add 1" ]

inputField : String -> String ->
              (String -> Msg) ->
              List (String, String) -> Html Msg
inputField default text updateItem inputStyle =
  input
    [ placeholder default, Attr.value text
    , onInput
    --, on "change" targetValue
        updateItem
    , style inputStyle
    ]
    []

myStyle : List (String, String)
myStyle =
  [ ("width", "100%")
  , ("height", "40px")
  , ("padding", "10px 0")
  , ("font-size", "2em")
  , ("text-align", "center")
  ]

textStyle : List (String, String)
textStyle =
  [ ("width", "100%")
  , ("height", "40px")
  , ("padding", "10px 0")
  , ("font-size", "2em")
  , ("text-align", "left")
  ]


-- converts Signal Model to Signal Html, using non-signal view
--main : Signal Html
--main = viewLift

main = HtmlApp.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--viewLift : Signal Html
--viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
view : Model -> Html Msg
view (i, s1, s2, s3, s4, s5, s6, s7) =
  div [class "container"]
  [
    addButton,    
    div [] [ text (toString i) ],
    div []
    [
      inputField "Files Query" s1 UpdateField myStyle
    ],
    div [ style textStyle] [ text ("first - " ++ s5) ],
    div [ style textStyle] [ text ("secPerms - " ++ s6) ],
    div [ style textStyle] [ text ("triPerms - " ++ s7) ]
  ]

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
--updateModelLift : Signal Model
--updateModelLift = Signal.foldp
--                    updateModel
--                    initialModel
--                    updatesChnl.signal

initialModel = (0, "xxx", "yyy", "zzz", "aaa", "r1", "r2", "r3")

init = (initialModel, Cmd.none)

-- converts Update to new Model
updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel update (i, s1, s2, s3, s4, s5, s6, s7) =
    case update of
      NoOp        -> ((i,      s1, s2, s3, s4, s1, s1, s1), Cmd.none)
      Add val     -> ((i + 1,  s1, s2, s3, s4, s1, s1, s1), Cmd.none)
      Remove val  -> ((i - 1,  s1, s2, s3, s4, s1, s1, s1), Cmd.none)

      UpdateField s ->  ((i + 2,   s, s2, s3, s4, s, s, s), Cmd.none)
      Circle2Field s -> ((i,   s1, s, s3, s4, s1, s1, s1), Cmd.none)
      Circle3Field s -> ((i,   s1, s2, s, s4, s1, s1, s1), Cmd.none)
      Circle4Field s -> ((i,   s1, s2, s3, s, s1, s1, s1), Cmd.none)


