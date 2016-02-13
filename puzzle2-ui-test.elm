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

--circle1Chnl : Signal.Mailbox String
--circle1Chnl : Signal.Mailbox Update
--circle1Chnl = Signal.mailbox (UpdateField "")

--circle2Chnl : Signal.Mailbox Update
--circle2Chnl = Signal.mailbox (Circle2Field "")

--circle3Chnl : Signal.Mailbox Update
--circle3Chnl = Signal.mailbox (Circle3Field "")

--circle4Chnl : Signal.Mailbox Update
--circle4Chnl = Signal.mailbox (Circle4Field "")


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
    div []
    [
      inputField "Files Query" s2 updatesChnlAddress Circle2Field myStyle
    ],
    div []
    [
      inputField "Files Query" s3 updatesChnlAddress Circle3Field myStyle
    ],
    div []
    [
      inputField "Files Query" s4 updatesChnlAddress Circle4Field myStyle
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
  let
    res       = s1 ++ ( toString ( circleNumsFromString s1 ) )
    secPerms  = s2 ++ (toString <| wheelPerms <| circleNumsFromString s2)
    thrPerms  = s3 ++ (toString <| wheelPerms <| circleNumsFromString s3)
    --res = s
  in
    case update of
      NoOp        -> (i,      s1, s2, s3, s4, res, secPerms, thrPerms)
      Add val     -> (i + 1,  s1, s2, s3, s4, res, secPerms, thrPerms)
      Remove val  -> (i - 1,  s1, s2, s3, s4, res, secPerms, thrPerms)

      UpdateField s ->  (i,   s, s2, s3, s4, res, secPerms, thrPerms)
      Circle2Field s -> (i,   s1, s, s3, s4, res, secPerms, thrPerms)
      Circle3Field s -> (i,   s1, s2, s, s4, res, secPerms, thrPerms)
      Circle4Field s -> (i,   s1, s2, s3, s, res, secPerms, thrPerms)


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