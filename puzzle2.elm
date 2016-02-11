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

type alias Model = (Int, String, String, String, String, String)

type Update =
      NoOp | Add Int | Remove Int |
      UpdateField String | Circle2Field String | Circle3Field String

type CircleChange = String

updatesChnl : Signal.Mailbox Update
updatesChnl = Signal.mailbox NoOp

--circle1Chnl : Signal.Mailbox String
--circle1Chnl : Signal.Mailbox Update
--circle1Chnl = Signal.mailbox (UpdateField "")

circle2Chnl : Signal.Mailbox Update
circle2Chnl = Signal.mailbox (Circle2Field "")

circle3Chnl : Signal.Mailbox Update
circle3Chnl = Signal.mailbox (Circle3Field "")

circle4Chnl : Signal.Mailbox String
circle4Chnl = Signal.mailbox ""


-- created by view, returns Html and sends Updates to updatesChnl
addButton : Html
addButton = Html.button
  [ Html.Events.onClick updatesChnl.address (Add 1)]
  [ Html.text "Add 1" ]

inputField : String -> String -> Signal.Address Update -> (String -> Update) -> Html
inputField default text chnlAddress updateItem =
  input
    [ placeholder default, Attr.value text
    , on "input" targetValue
        (Signal.message chnlAddress << updateItem)
    , style myStyle
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


-- converts Signal Model to Signal Html, using non-signal view
main : Signal Html
main = viewLift

viewLift : Signal Html
viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
view : Signal.Address Update -> Model -> Html
view updatesChnlAddress (i, s1, s2, s3, s4, s5) =
  div [class "container"]
  [
    addButton,    
    div [] [ text (toString i) ],
    div []
    [
      inputField "Files Query" s1 updatesChnlAddress UpdateField
      --input
      --  [ placeholder "Files Query"
      --  , Attr.value s1
      --  , on "input" targetValue
      --      (Signal.message updatesChnlAddress << UpdateField)
      --  , style myStyle
      --  ]
      --  []
    ],
    div []
    [
      input
        [ placeholder "Files Query"
        , Attr.value s2
        , on "input" targetValue
          (Signal.message updatesChnl.address << Circle2Field)
        , style myStyle
        ]
        []
    ],
    div []
    [
      input
        [ placeholder "Files Query"
        , Attr.value s3
        , on "input" targetValue
          (Signal.message updatesChnl.address << Circle3Field)
        , style myStyle
        ]
        []
    ],
     div []
     [
       input
         [ placeholder "Files Query"
         , Attr.value s4
         , on "input" targetValue (Signal.message circle4Chnl.address)
         , style myStyle
         ]
         []
     ],
       div []
       [
         input
           [ placeholder "Files Query"
           , Attr.value s5
           -- , on "input" targetValue (Signal.message circle4Chnl.address)
           , style myStyle
           ]
           []
       ]
  ]

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
updateModelLift : Signal Model
updateModelLift = Signal.foldp
                    updateModel
                    (0, "xxx", "yyy", "zzz", "aaa", "rrr")
                    updatesChnl.signal

-- converts Update to new Model
updateModel : Update -> Model -> Model
updateModel update (i, s1, s2, s3, s4, s5) =
    let
        res = toString ( circleNumsFromString s1 )
        res2 = toString ( circleNumsFromString s2 )
        res3 = toString <| wheelPerms <| circleNumsFromString s3
        --res = s
    in
      case update of
        NoOp        -> (i,      s1, s2, s3, s4, res)
        Add val     -> (i + 1,  s1, s2, s3, s4, res)
        Remove val  -> (i - 1,  s1, s2, s3, s4, res)

        UpdateField s -> (i,    s, s2, s3, s4, res)
        Circle2Field s -> (i,   s1, s, s3, s4, res2)
        Circle3Field s -> (i,   s1, s2, s, s4, res3)


circleNumsFromString : String -> List Int
--circleNumsFromString : String -> List String
-- circleNumsFromString s = [1]
circleNumsFromString s =
    List.map
      strToNum
        (String.split "," s)

strToNum : String -> Int
strToNum s = resToNum (String.toInt s)

resToNum : Result e Int -> Int
resToNum r =
    case r of
        Ok x -> x
        Err s -> 0


-- PUZZLE SOLUTIONS

listLoopItem list chunk = (drop chunk list) ++ (take chunk list)

listLoops : List (List Int) -> List Int -> Int -> List (List Int)
listLoops lists seed count =
  case count of
    0 ->
      lists ++ [seed]
    otherwise ->
      listLoops (lists ++ [listLoopItem seed count]) seed (count-1)

wheelPerms : List Int -> List (List Int)
wheelPerms xs = listLoops [] xs ( (length xs) - 1 )
