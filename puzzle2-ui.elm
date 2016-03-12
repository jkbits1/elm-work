-- NOTE: code comes from Signal.send package example
-- have changed fn names to show lifting fns (not sure if it is clearer)

--  <link rel="stylesheet" href="css/tidy.css">
-- <link rel="stylesheet" href="css/bootstrap.css">

import PuzzleModule exposing (..)

import Signal

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Graphics.Input.Field as Field
import Graphics.Element exposing (..)

import String
import List exposing (..)


-- values received from UI
type alias ModelInputs  = (Int, String, String, String, String)
type alias ModelButtons = (Bool, Bool, Bool, Bool, Bool, Bool, Bool)

-- values generated from UI input
type alias ModelResults =
  (WheelPosition, WheelLoop, WheelLoop, WheelLoop,
    List LoopsPermutation,                      List LoopsPermutation,
    (List (LoopsPermAnswers, LoopsPermutation), List (LoopsPermAnswers, LoopsPermutation),
      List (LoopsAnswerLoop, LoopsPermutation), List (LoopsAnswerLoop, LoopsPermutation)
      , (LoopsPermAnswers, LoopsPermutation)
      ))

  --(firstList, secList, thrList, ansList, twoListPerms, threeListPerms,
   --(ansPlusList, specificAnswer, ansPermsPlusList, specificAnswerPlusList))

type alias Model = (ModelList, ModelInputs, ModelButtons, ModelResults)

type alias ModelList = List (ModelInputs, ModelButtons)

type Update =
      NoOp | Add Int | Remove Int |
      Back |
      UpdateField String |
      Circle1Field String |
      Circle2Field String | Circle3Field String | Circle4Field String |
      ShowLoop2 | ShowLoop3 | ShowLoopAns |
      ShowPerms2 | ShowPerms3 |
      ShowAns | ShowLazyAns

--type CircleChange = String

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

buttonClassList = classList [("btn", True), ("btn-default", True)]

-- created by view, returns Html and sends Updates to updatesChnl
answersButton : Html
answersButton = uiButton ShowAns  "Show Answers"

backButton : Html
backButton    = uiButton Back     "Step Back"

stateButton : Html
stateButton   = uiButton Back     "Show State"

perms2Button : Html
perms2Button  = uiButton ShowPerms2 "Show Perms 2"

perms3Button : Html
perms3Button  = uiButton ShowPerms3 "Show Perms 3"


uiButton : Update -> String -> Html
uiButton action label = Html.button
  [
      buttonClassList
    , Html.Events.onClick updatesChnl.address action]
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
        , Html.Events.onClick updatesChnl.address action
      ]
      [ Html.text label ]


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

inputField2 : String -> String -> String ->
              Signal.Address Update -> (String -> Update) ->
              List (String, String) -> Html
inputField2 idVal default text chnlAddress updateItem inputStyle =
  input
    [ placeholder default, Attr.value text
    , on "input" targetValue
    --, on "change" targetValue
        (Signal.message chnlAddress << updateItem)
    --, style inputStyle
      , style wheelStyle
      , id idVal
      , class "form-control col-sm-2"
    ]
    []

formGroup lbl idVal val chnlAddress updateItem style =
  div [class "form-group"] [
    --div [class "col-sm-2"] [
        label [ for idVal,
                --classList [("control-label", True),("col-sm-4", True)]
                classList [("control-label", True),("col-sm-4", True)]
                ]
                [text lbl]
      , inputField2 idVal lbl val chnlAddress updateItem style
    --]
  ]


wheelOnlyRow idx wheelLabel wheelData =
    div [class "row"] [

      -- , style "background-color: #00b3ee"
      div [class "col-sm-2"] [
        text wheelLabel
      ]
      ,
      div [class "col-sm-2"] [
        text <| wheelData
      ]

    ]

wheelRow idx wheelLabel loopLabel wheelData loopData action hide =
    div [class "row", style [("min-height", "50px"), ("margin-top", "10px")]] [

      -- , style "background-color: #00b3ee"
      div [class "col-sm-2"] [
        text wheelLabel
      ]
      ,
      div [class "col-sm-2"] [
        text <| wheelData
      ]
      ,
      div [class "col-sm-2"] [
        -- <button type="button" class="btn btn-default">Hide</button>
        -- showLoopButton ("Show", "Hide") hide action
        showLoopButton ("+", "-") hide action
      ]
      ,
      div [class "col-sm-2", style <| displayStyle hide ] [
        text loopLabel
      ]

      ,
      div [class "col-sm-2", style <| displayStyle hide ] [
        text loopData
      ]
    ]

foundAnswerIndicator answerList show =
  let found =
    if (length answerList == 0) then
      "No"
    else
      "Yes"
  in
    div [ style <| (displayStyle show) ++ [("font-weight", "700")] ]
    [
      text <| "foundAnswer? - " ++ found
    ]


wheelStyle : List (String, String)
wheelStyle =
  [
    -- ("width", "100px")
    ("min-width", "200px")
  ]

myStyle : List (String, String)
myStyle =
  [ ("width", "100%")
  , ("height", "40px")
  , ("padding", "10px 0")
  --, ("font-size", "2em")
  , ("font-size", "1.8em")
  , ("text-align", "center")
  ]

textStyle : List (String, String)
textStyle =
  [ ("width", "100%")
  --, ("height", "70px")
  , ("padding", "10px 0")
  --, ("font-size", "2em")
  , ("font-size", "1.8em")
  , ("text-align", "left")
  ]

displayStyle : Bool -> List (String, String)
displayStyle show =
  case show of
    True ->   [("display", "block")]
    False ->  [("display", "none")]

-- converts Signal Model to Signal Html, using non-signal view
main : Signal Html
main = viewLift

viewLift : Signal Html
viewLift = Signal.map (view updatesChnl.address) updateModelLift

-- used by main, as a non-signal function, to convert a Model to Html
view : Signal.Address Update -> Model -> Html
view updatesChnlAddress ( stateHistory,
                          (i, s1, s2, s3, s4),
                          (b1, b2, b3, b4, b5, b6, b7),
                          (firstList, secLoop, thrLoop, ansLoop, twoListPerms, threeListPerms,
                            (ansPlusList, specificAnswer, ansPermsPlusList, specificAnswerPlusList
                              , findAnswerLazy3))
                        ) =
  div [] [
  div [class "container"]
  [
      div [class "row"] [
        div [class "btn-group"] [
            answersButton
          , backButton
          , stateButton
          , perms2Button
          , perms3Button
        ]
      ]
    , br [] []

    , Html.form [class "form-inline"][
      --div [class "row"] [
        formGroup "Wheel 1" "wheel1input"     s1 updatesChnlAddress Circle1Field myStyle
      , formGroup "Wheel 2" "wheel2input"     s2 updatesChnlAddress Circle2Field myStyle
      , formGroup "Wheel 3" "wheel3input"     s3 updatesChnlAddress Circle3Field myStyle
      , formGroup "Wheel Ans" "wheelAnsInput" s4 updatesChnlAddress Circle4Field myStyle
    ]

    , br [] []

    , wheelOnlyRow  1 "Wheel 1"               s1 -- (toString firstList)
    , wheelRow      2 "Wheel 2"   "Loop 2"    s2 (toString secLoop) ShowLoop2   b2
    , wheelRow      3 "Wheel 3"   "Loop 3"    s3 (toString thrLoop) ShowLoop3   b3
    , wheelRow      4 "Wheel Ans" "Loop Ans"  s4 (toString ansLoop) ShowLoopAns b4
  ]

  , br [] []

  , div [class "container"]
  [
      div [class "row"] [
          div [ style -- textStyle
                  (displayStyle b5)
          ] [ text ("2loopPerms - " ++ (toString twoListPerms) ++ (toString b5)) ]
      ]
    , div [class "row"] [
        div [ style -- textStyle
                (displayStyle b6)
        ] [ text ("3loopPerms - " ++ (toString threeListPerms)) ]
      ]
    , div [class "row"] [
        div [ --style textStyle
        ] [
            div [ style
                  -- <| textStyle ++
                  (displayStyle b1)]
            [ text ("answersPlus - " ++ (toString ansPlusList)) ]
          , foundAnswerIndicator specificAnswer b1
          , div [ style
                  -- <| textStyle ++
                  (displayStyle b1)]
            [ text ("findAnswers - " ++ (toString specificAnswer)) ]
        ]
      ]
    , br [] []
    , div [class "row"] [
        text <| toString stateHistory
      ]

    , div [] [ text (toString i) ]
    , div [] [ text (toString b1) ]
    --div []
    --[
    --  inputField "Files Query" s1 updatesChnlAddress UpdateField myStyle
    --],
    --div []
    --[
    --  inputField "Files Query" s2 updatesChnlAddress Circle2Field myStyle
    --],
    --div []
    --[
    --  inputField "Files Query" s3 updatesChnlAddress Circle3Field myStyle
    --],
    --div []
    --[
    --  inputField "Files Query" s4 updatesChnlAddress Circle4Field myStyle
    --],
    -- div [ style textStyle] [ text ("first  - " ++ (toString firstList)) ],
    -- div [ style textStyle] [ text ("secLoop - " ++ (toString secLoop)) ],
    -- div [ style textStyle] [ text ("thrLoop - " ++ (toString thrLoop)) ],
    -- div [ style textStyle] [ text ("ansLoop - " ++ (toString ansLoop)) ],
    , div [ style <| textStyle ++ (displayStyle False)] [ text ("answersPerms - " ++ (toString ansPermsPlusList)) ]
    , div [ style <| textStyle ++ (displayStyle False)] [ text ("displayAnswer - " ++ (toString specificAnswerPlusList)) ]
    , div [ style <| textStyle ++ (displayStyle b1)] [ text ("lazyAnswer - " ++ (toString findAnswerLazy3)) ]

    , div [class "row"] [
      text <| toString stateHistory
    ]

    , div [class "row"] [
    ]
  ]
  ]

      -- <input type="text" placeholder="wheel1">
      -- <input type="text" placeholder="wheel2">
      -- <input type="text" placeholder="wheel3">
      -- <input type="text" placeholder="wheelAns">



-- candidates for viewHelperFns
--

--secPermsShow : String -> String
--secPermsShow  = (\s2 -> s2 ++ " " ++ (toString <| getSecLoop s2) )

--thrPermsShow : String -> String
--thrPermsShow = (\s3 -> s3 ++ " " ++ (toString <| makeThrLoop s3) )

--ansPermsShow : String -> String
--ansPermsShow = (\s4 -> s4 ++ " " ++ (toString <| getAnsLoop s4) )

-- innerShow = s1 ++ " " ++ (toString first)
-- twoListPermsShow =            toString <| twoListPerms            first s2
-- threeListPermsShow =          toString <| threeListPerms          first s2 s3
-- answersPlusPermShow =         toString <| answersPlusPerm         first s2 s3
-- findSpecificAnswerShow =      toString <| findSpecificAnswer      first s2 s3 <| getAnsLoop s4
-- answersPermsPlusListShow =    toString <| answersPermsPlusList    first s2 s3
-- displaySpecificAnswersShow =  toString <| displaySpecificAnswers  first s2 s3 answers

initialInputs = (0, "1,2,3", "4,5,6", "7,8,9", "12,15,18")
initialStates = (False, False, False, False, False, False, False)
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

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
updateModelLift : Signal Model
updateModelLift = Signal.foldp
                    updateModel
                    initialModelState
                    updatesChnl.signal

-- converts Update to new Model
updateModel : Update -> Model -> Model
updateModel update (stateHistory, (i, s1, s2, s3, s4),
                     (b1, b2, b3, b4, b5, b6, b7),
                     --(xs, xxs2, xxs3, xxs4, s9, s10, (s11, s12, s13, s14))
                     results
                   ) =
  let
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
        first     = wheelPositionFromString s1
        answers   = wheelPositionFromString s4
        secLoop   = makeSecLoop s2
        thrLoop   = makeThrLoop s3
        ansLoop   = makeAnsLoop s4
        inputs    = (i, s1, s2, s3, s4)
        newCalcs  = (first, secLoop, thrLoop, ansLoop,
                      twoWheelPerms first secLoop, threeLoopPerms first secLoop thrLoop,
                      (answersPlusPerm      first secLoop thrLoop,
                        findSpecificAnswer  first secLoop thrLoop ansLoop,
                        answersPermsPlusList first secLoop thrLoop,
                        displaySpecificAnswers first secLoop thrLoop answers
                        , findAnswerLazy3 first secLoop thrLoop ansLoop))
      in
        (newHistory, inputs, buttonStates, newCalcs)
  in
    case update of
      NoOp        ->    createModel  (i,      s1, s2, s3, s4) (b1, b2, b3, b4, b5, b6, b7) True
      -- Add val     ->    createModel ((i + 1), s1, s2, s3, s4) (not b1, b2, b3, b4, b5)  True
      -- Remove val  ->    createModel ((i - 1), s1, s2, s3, s4) (True, b2, b3, b4, b5)    True

      Back        ->    createModel inputs states False

      Circle1Field s -> createModel  (i,      s,  s2, s3, s4) (b1, b2, b3, b4, b5, b6, b7) True
      Circle2Field s -> createModel  (i,      s1, s,  s3, s4) (b1, b2, b3, b4, b5, b6, b7) True
      Circle3Field s -> createModel  (i,      s1, s2, s,  s4) (b1, b2, b3, b4, b5, b6, b7) True
      Circle4Field s -> createModel  (i,      s1, s2, s3, s)  (b1, b2, b3, b4, b5, b6, b7) True

      ShowAns     ->    createModel ((i + 1), s1, s2, s3, s4) (not b1, b2, b3, b4, b5, b6, b7) True
      ShowLoop2   ->    createModel ((i + 1), s1, s2, s3, s4) (b1, not b2, b3, b4, b5, b6, b7) True
      ShowLoop3   ->    createModel ((i + 1), s1, s2, s3, s4) (b1, b2, not b3, b4, b5, b6, b7) True
      ShowLoopAns ->    createModel ((i + 1), s1, s2, s3, s4) (b1, b2, b3, not b4, b5, b6, b7) True

      ShowPerms2 ->    createModel ((i + 1), s1, s2, s3, s4) (b1, b2, b3, b4, not b5, b6, b7) True
      ShowPerms3 ->    createModel ((i + 1), s1, s2, s3, s4) (b1, b2, b3, b4, b5, not b6, b7) True


