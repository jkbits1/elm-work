--  <link rel="stylesheet" href="css/tidy.css">
-- <link rel="stylesheet" href="css/bootstrap.css">

port module WheelApp exposing (..)

import PuzzleModule exposing (..)
import Wheel exposing (..)

import Html exposing (..)
import Html.App as HtmlApp
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import String
import List exposing (..)


type alias ModelInputs  = (Int, String, String, String, String)
type alias ModelButtons = List Bool

-- values generated from UI input
type alias ModelResults =
  (WheelPosition, WheelLoop, WheelLoop, WheelLoop,
    List LoopsPermutation,                      List LoopsPermutation,
    (List (LoopsPermAnswers, LoopsPermutation), List (LoopsPermAnswers, LoopsPermutation),
      List (LoopsAnswerLoop, LoopsPermutation), List (LoopsAnswerLoop, LoopsPermutation)
      , (LoopsPermAnswers, LoopsPermutation)
      ))

type alias Model = (ModelList, ModelInputs, ModelButtons, ModelResults)

type alias ModelList = List (ModelInputs, ModelButtons)

type Msg =
      NoOp |
--      Add Int | Remove Int |
      Back |
--      UpdateField String |
      Circle1Field String |
      Circle2Field String | Circle3Field String | Circle4Field String |
      ShowLoop2 | ShowLoop3 | ShowLoopAns |
      ShowPerms2 | ShowPerms3 |
      ShowAns |
--      ShowLazyAns |
      ShowState |
      ChangeWheel |
      D3Response (List String)

buttonVal : List Bool -> Int -> Bool
buttonVal list num =
  let
    h = head (drop (num - 1) list)
  in
    case h of
      Just x  -> x
      Nothing -> False

-- max buttons is 9, 10 items in list
maxButton = 10
buttonListToggle list num = take (num-1) list ++ [not <| buttonVal list num] ++ take (maxButton - num) (drop num list)


buttonClassList = classList [("btn", True), ("btn-default", True)]

backButton : Html Msg
backButton    = uiButton Back       "Step Back"

answersButton : Bool -> Html Msg
answersButton hide  = showLoopButton ("Show Answers", "Hide Answers") hide ShowAns

stateButton : Bool -> Html Msg
stateButton  hide   = showLoopButton ("Show State", "Hide State")     hide ShowState

perms2Button : Bool -> Html Msg
perms2Button hide   = showLoopButton ("Show Perms 2", "Hide Perms 2") hide ShowPerms2

perms3Button : Bool -> Html Msg
perms3Button hide   = showLoopButton ("Show Perms 3", "Hide Perms 3") hide ShowPerms3


uiButton : Msg -> String -> Html Msg
uiButton action label = Html.button
  [
      buttonClassList
    , Html.Events.onClick action]
  [ Html.text label ]

--showLoopButton : Html
showLoopButton labels hide action =
  let
    label =
      if hide == True then
        snd labels
      else
        fst labels
  in
    Html.button
      [   buttonClassList
        , Html.Events.onClick action
      ]
      [ Html.text label ]


inputField : String -> String ->
              (String -> Msg) ->
              List (String, String) -> Html Msg
inputField default text updateItem inputStyle =
  input
    [ placeholder default, Attr.value text
    , onInput updateItem
    , style inputStyle
    ]
    []

inputField2 : String -> String -> String ->
              (String -> Msg) ->
              List (String, String) -> Html Msg
inputField2 idVal default text updateItem inputStyle =
  input
    [ placeholder default, Attr.value text
    , onInput updateItem
      , style wheelStyle
      , id idVal
      , class "form-control col-sm-2"
    ]
    []

formGroup lbl idVal val updateItem style =
  div [class "form-group"] [
        label [ for idVal,
                --classList [("control-label", True),("col-sm-4", True)]
                classList [("control-label", True),("col-sm-4", True)]
                ]
                [text lbl]
      , inputField2 idVal lbl val updateItem style
  ]

wheelOnlyRow idx wheelLabel wheelData =
    div [class "row"] [
      div [class "col-sm-2"] [ text wheelLabel ]
      ,
      div [class "col-sm-2"] [ text <| wheelData ]
    ]

wheelRow idx wheelLabel loopLabel wheelData loopData action hide =
    div [class "row", style [("min-height", "50px"), ("margin-top", "10px")]] [

        div [class "col-sm-2", style [("font-weight", "700")] ] [ text wheelLabel ]
      , div [class "col-sm-2"] [ text <| wheelData ]
      , div [class "col-sm-2"] [ showLoopButton ("+", "-") hide action ]
      , div [class "col-sm-2", style <| (displayStyle hide) ++ [("font-weight", "700")] ] [
        text loopLabel
      ]

      , div [class "col-sm-2", style <| displayStyle hide ] [ text loopData ]
    ]

foundAnswerIndicator answerList show =
  let found =
    if (length answerList == 0) then
      "No"
    else
      "Yes"
  in
    div [ style <| (displayStyle show) ++ [("font-weight", "700")] ]
    [ text <| "foundAnswer? - " ++ found ]


wheelStyle : List (String, String)
wheelStyle =
  [ ("min-width", "200px") ]

myStyle : List (String, String)
myStyle =
  [ ("width", "100%")
  , ("height", "40px")
  , ("padding", "10px 0")
  , ("font-size", "1.8em")
  , ("text-align", "center")
  ]

textStyle : List (String, String)
textStyle =
  [ ("width", "100%")
  , ("padding", "10px 0")
  , ("font-size", "1.8em")
  , ("text-align", "left")
  ]

displayStyle : Bool -> List (String, String)
displayStyle show =
  case show of
    True ->   [("display", "block")]
    False ->  [("display", "none")]

infoRow label info displayState =
  div [class "row", style <| -- textStyle
                      [("margin-top", "10px")] ++
                      (displayStyle displayState)] [
      div [class "col-sm-2"] [ text label ]
    , div [class "col-sm-8"] [ text <| info ]
  ]


-- converts Signal Model to Signal Html, using non-signal view
--main : Signal Html
--main = viewLift

main = HtmlApp.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

-- subscriptions, data responses from js
port dataProcessedItems : (List String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
--subscriptions model = Sub.none
subscriptions model = dataProcessedItems D3Response

--viewLift : Signal Html
--viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
view : Model -> Html Msg
view ( stateHistory,
        (i, s1, s2, s3, s4),
        buttonList,
        (firstList, secLoop, thrLoop, ansLoop, twoListPerms, threeListPerms,
          (ansPlusList, specificAnswer, ansPermsPlusList, specificAnswerPlusList
            , findAnswerLazy3))
      ) =
  div [] [
  div [class "container"]
  [
         div [class "row"] [
          h2 [] [text "Elm Calculations"]
         ]

       , div [class "row"] [
          div [class "btn-group"] [
              perms2Button  <| buttonVal buttonList 5
            , perms3Button  <| buttonVal buttonList 6
            , answersButton <| buttonVal buttonList 1
          ]
        , div [class "btn-group", style [("margin-left", "5px")] ] [
            backButton
          , stateButton <| buttonVal buttonList 7
        ]
      ]
    , br [] []

    , Html.form [class "form-inline"][
        formGroup "Wheel 1" "wheel1input"     s1 Circle1Field myStyle
      , formGroup "Wheel 2" "wheel2input"     s2 Circle2Field myStyle
      , formGroup "Wheel 3" "wheel3input"     s3 Circle3Field myStyle
      , formGroup "Wheel Ans" "wheelAnsInput" s4 Circle4Field myStyle
    ]

    , br [] []

    , wheelOnlyRow  1 "Wheel 1"               s1
    , wheelRow      2 "Wheel 2"   "Loop 2"    s2 (toString secLoop) ShowLoop2   <| buttonVal buttonList 2
    , wheelRow      3 "Wheel 3"   "Loop 3"    s3 (toString thrLoop) ShowLoop3   <| buttonVal buttonList 3
    , wheelRow      4 "Wheel Answers" "Loop Answers"  s4 (toString ansLoop) ShowLoopAns <| buttonVal buttonList 4
  ]

  , br [] []

  , div [class "container"]
  [
      div [class "row"
             , style (displayStyle <| buttonVal buttonList 1)
      ] [
          div [ class "col-sm-12" ] [ foundAnswerIndicator specificAnswer <| buttonVal buttonList 1 ]
      ]

    , br [] []
    ,  infoRow "2 Loop Perms"     (toString twoListPerms)   <| buttonVal buttonList 5
    , infoRow "3 Loop Perms"  (toString threeListPerms) <| buttonVal buttonList 6
    , infoRow "answersPlus"  (toString ansPlusList ) <| buttonVal buttonList 1

    , br [] []

    , div [class "row", style (displayStyle <| buttonVal buttonList 1)] [
          div [ class "col-sm-2" ] [ text "findAnswers - " ]
        , div [ class "col-sm-8" ] [ text <| toString specificAnswer ]
      ]

    , infoRow "lazyAnswer - " (toString findAnswerLazy3) <| buttonVal buttonList 1
    , infoRow ("State change count: " ++ (toString i)) (toString stateHistory) <| buttonVal buttonList 7

    , div [ style <| textStyle ++ (displayStyle False)] [ text ("answersPerms - " ++ (toString ansPermsPlusList)) ]
    , div [ style <| textStyle ++ (displayStyle False)] [ text ("displayAnswer - " ++ (toString specificAnswerPlusList)) ]

    , div [class "row"] [
    ]
    , div [class "row"] [
        Html.button
          [
            Html.Events.onClick ChangeWheel
          ]
          [ Html.text "wheel" ]
    ]
  ]
  ]

-- outgoing port to js
port check : List (List WheelItem) -> Cmd msg

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
--updateModelLift : Signal Model
--updateModelLift = Signal.foldp
--                    updateModel
--                    initialModelState
--                    updatesChnl.signal

-- converts Update to new Model
updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel update (stateHistory, (i, s1, s2, s3, s4),
                     buttonList,
                     --(xs, xxs2, xxs3, xxs4, s9, s10, (s11, s12, s13, s14))
                     results
                   ) =
  let
    newCount   = i + 1
    (inputs, states) = Maybe.withDefault
                  (initialInputs, initialStates)
                  (head stateHistory)
    tailHistory     = Maybe.withDefault [] (tail stateHistory)

    createModel (i, s1, s2, s3, s4) buttonStates forward =
      let
        newHistory =
          if forward == True then
            (inputs, buttonStates) :: stateHistory
          else
            tailHistory

        inputs    = (i, s1, s2, s3, s4)

        first     = wheelPositionFromString s1
        answers   = wheelPositionFromString s4
        secLoop   = makeSecLoop s2
        thrLoop   = makeThrLoop s3
        ansLoop   = makeAnsLoop s4

        newCalcs  = (first, secLoop, thrLoop, ansLoop,
                      twoWheelPerms first secLoop, threeLoopPerms first secLoop thrLoop,
                      (answersPlusPerm      first secLoop thrLoop,
                        findSpecificAnswer  first secLoop thrLoop ansLoop,
                        answersPermsPlusList first secLoop thrLoop,
                        displaySpecificAnswers first secLoop thrLoop answers
                        , findAnswerCS first secLoop thrLoop ansLoop))
      in
        (newHistory, inputs, buttonStates, newCalcs)
  in
    case update of
      NoOp        ->    (createModel (i,       s1, s2, s3, s4) buttonList True, Cmd.none)

      Back        ->    (createModel inputs states False, Cmd.none)

      Circle1Field s -> (createModel(newCount, s,  s2, s3, s4) buttonList True, Cmd.none)
      Circle2Field s -> (createModel(newCount, s1, s,  s3, s4) buttonList True, Cmd.none)
      Circle3Field s -> (createModel(newCount, s1, s2, s,  s4) buttonList True, Cmd.none)
      Circle4Field s -> (createModel(newCount, s1, s2, s3, s)  buttonList True, Cmd.none)

      --ShowAns     ->    (createModel((i + 1), s1, s2, s3, s4) (not b1, b2, b3, b4, b5, b6, b7) True
      ShowAns     ->    (createModel(newCount, s1, s2, s3, s4) (buttonListToggle buttonList 1) True, Cmd.none)
      ShowLoop2   ->    (createModel(newCount, s1, s2, s3, s4) (buttonListToggle buttonList 2) True, Cmd.none)
      ShowLoop3   ->    (createModel(newCount, s1, s2, s3, s4) (buttonListToggle buttonList 3) True, Cmd.none)
      ShowLoopAns ->    (createModel(newCount, s1, s2, s3, s4) (buttonListToggle buttonList 4) True, Cmd.none)

      ShowPerms2 ->     (createModel(newCount, s1, s2, s3, s4) (buttonListToggle buttonList 5) True, Cmd.none)
      ShowPerms3 ->     (createModel(newCount, s1, s2, s3, s4) (buttonListToggle buttonList 6) True, Cmd.none)

      ShowState ->      (createModel(newCount, s1, s2, s3, s4) (buttonListToggle buttonList 7) True, Cmd.none)

      ChangeWheel ->
        let
          mdl = createModel (i,       s1, s2, s3, s4) buttonList True
          wd1 = resultsToD3Data <| wheelData mdl
        in
          (mdl
          , check [
--                  [{name = "1"}]
                    wd1
                  , wd1
                  , wd1
                  ]
          )

      -- currently a no-op
      D3Response rs -> (createModel (i,       s1, s2, s3, s4) buttonList True, Cmd.none)


initialInputs = (0, "1,2,3", "4,5,6", "7,8,9", "12,15,18")
initialStates = [False, False, False, False, False, False, False, False, False, False]
initialCalcs  =
    ([1,2,3], [[4,5,6]], [[7,8,9]], [[12,15,18]], [[[2]]], [[[3]]],
      ([([1], [[1]])], [([1], [[1]])], [([[1]], [[1]])], [([[1]], [[1]])]
      ,([1], [[1]])
      )
    )

initialModelState =
  ([],
      initialInputs,
    --(0, "6,5,5,6,5,4,5,4", "4,2,2,2,4,3,3,1", "1,3,2,3,3,2,4,3",
      --    "12,8,12,10,10,12,10,8"),
    initialStates,
    initialCalcs
  )

modelInputs
      ( stateHistory,
        inputs,
        buttonList,
        results
      ) = inputs

init = (initialModelState, Cmd.none)

input1 (_, i1, _, _, _) = i1

wheelData : Model -> List Int
wheelData model = wheelPositionFromString <| input1 <| modelInputs model

resultsToD3Data : List Int -> List { name: String }
resultsToD3Data xs = List.map (\x -> { name = (toString x) }) xs

--type alias ModelResults =
--  (WheelPosition, WheelLoop, WheelLoop, WheelLoop,
--    List LoopsPermutation,                      List LoopsPermutation,
--    (List (LoopsPermAnswers, LoopsPermutation), List (LoopsPermAnswers, LoopsPermutation),
--      List (LoopsAnswerLoop, LoopsPermutation), List (LoopsAnswerLoop, LoopsPermutation)
--      , (LoopsPermAnswers, LoopsPermutation)
--      ))



--type Msg2 =
--  Change String
--  | Check
--  | Suggest (List String)

-- subscriptions, data responses from js
--port dataProcessedItems : (List String -> msg) -> Sub msg


-- copy of puzzle subs
--subscriptions : Model -> Sub Msg
--subscriptions model = Sub.none

--subscriptions : Model -> Sub Msg
--subscriptions model = dataProcessedItems Suggest


