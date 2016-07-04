-- NOTE: code comes from Signal.send package example
-- have changed fn names to show lifting fns (not sure if it is clearer)

--import Signal (channel)
import Signal
--import Html
--import Html exposing (Html, div, text, input, placeholder)
--import Html.Attributes exposing (class)
--import Html.Events

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Graphics.Input.Field as Field
import Graphics.Element exposing (..)

import String
import List exposing (..)

type alias Model = (Int, String, String, String, String,
                         String, String, String)

type Update =
      NoOp | Add Int | Remove Int |
      UpdateField String | Circle2Field String | Circle3Field String | Circle4Field String

type CircleChange = String

updatesChnl : Signal.Mailbox Update
updatesChnl = Signal.mailbox NoOp


-- created by view, returns Html and sends Updates to updatesChnl
addButton : Html
addButton = Html.button
  [ Html.Events.onClick updatesChnl.address (Add 1)]
  [ Html.text "Add 1" ]

inputField : String -> String ->
              Signal.Address Update -> (String -> Update) ->
              List (String, String) -> Html
inputField default text chnlAddress updateItem inputStyle =
  input
    [ placeholder default, Attr.value text
    , on "input" targetValue
    --, on "change" targetValue
        (Signal.message chnlAddress << updateItem)
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
main : Signal Html
main = viewLift

viewLift : Signal Html
viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
view : Signal.Address Update -> Model -> Html
view updatesChnlAddress (i, s1, s2, s3, s4, s5, s6, s7) =
  div [class "container"]
  [
    addButton,    
    div [] [ text (toString i) ],
    div []
    [
      inputField "Files Query" s1 updatesChnlAddress UpdateField myStyle
    ],
    div [ style textStyle] [ text ("first - " ++ s5) ],
    div [ style textStyle] [ text ("secPerms - " ++ s6) ],
    div [ style textStyle] [ text ("triPerms - " ++ s7) ]
  ]

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
updateModelLift : Signal Model
updateModelLift = Signal.foldp
                    updateModel
                    (0, "xxx", "yyy", "zzz", "aaa", "r1", "r2", "r3")
                    updatesChnl.signal

-- converts Update to new Model
updateModel : Update -> Model -> Model
updateModel update (i, s1, s2, s3, s4, s5, s6, s7) =
    case update of
      NoOp        -> (i,      s1, s2, s3, s4, s1, s1, s1)
      Add val     -> (i + 1,  s1, s2, s3, s4, s1, s1, s1)
      Remove val  -> (i - 1,  s1, s2, s3, s4, s1, s1, s1)

      UpdateField s ->  (i + 2,   s, s2, s3, s4, s, s, s)
      Circle2Field s -> (i,   s1, s, s3, s4, s1, s1, s1)
      Circle3Field s -> (i,   s1, s2, s, s4, s1, s1, s1)
      Circle4Field s -> (i,   s1, s2, s3, s, s1, s1, s1)


