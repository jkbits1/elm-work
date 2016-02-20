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

type alias WheelPosition    = List Int           -- a single position of a wheel
type alias WheelLoop        = List WheelPosition -- all positions of a wheel
type alias LoopsPermutation = List (List Int) -- combo of one or more loops
type alias LoopsPermAnswer  = List Int        -- results of combo addition
type alias LoopsAnswerLoop  = List WheelPosition      -- all turns of combo addition
type alias LoopsPermColumn  = (Int, Int, Int)  -- values from a single loops perm

-- values received from UI
type alias ModelInputs  = (Int, String, String, String, String)

-- values generated from UI input
type alias ModelResults =
  (WheelPosition, WheelLoop, WheelLoop, WheelLoop,
    List LoopsPermutation,                       List LoopsPermutation,
    (List (LoopsPermAnswer, LoopsPermutation),       List (LoopsPermAnswer, LoopsPermutation),
      List (LoopsAnswerLoop, LoopsPermutation), List (LoopsAnswerLoop, LoopsPermutation)))

  --(firstList, secList, thrList, ansList, twoListPerms, threeListPerms,
   --(ansPlusList, specificAnswer, ansPermsPlusList, specificAnswerPlusList))

type alias Model = (ModelInputs, (Bool), ModelResults)

type Update =
      NoOp | Add Int | Remove Int |
      UpdateField String |
      Circle2Field String | Circle3Field String | Circle4Field String

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
  [ Html.text "Show Answers" ]

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
view updatesChnlAddress (
                          (i, s1, s2, s3, s4),
                          (b1),
                          (firstList, secList, thrList, ansList, twoListPerms, threeListPerms,
                            (ansPlusList, specificAnswer, ansPermsPlusList, specificAnswerPlusList))
                        ) =
  div [class "container"]
  [
    addButton,    
    div [] [ text (toString i) ],
    div [] [ text (toString b1) ],
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
    div [ style textStyle] [ text ("first xxxxx - " ++ (toString firstList)) ],
    div [ style textStyle] [ text ("secPerms - " ++ (toString secList)) ],
    div [ style textStyle] [ text ("triPermsx - " ++ (toString thrList)) ],
    div [ style textStyle] [ text ("ansPermsx - " ++ (toString ansList)) ],
    div [ style textStyle] [ text ("2listPermsx - " ++ (toString twoListPerms)) ],
    div [ style textStyle] [ text ("3listPermsx - " ++ (toString threeListPerms)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("answersPlus - " ++ (toString ansPlusList)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("findAnswers - " ++ (toString specificAnswer)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("answersPerms - " ++ (toString ansPermsPlusList)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("displayAnswer - " ++ (toString specificAnswerPlusList)) ]
  ]

-- candidates for viewHelperFns
--

--secPermsShow : String -> String
--secPermsShow  = (\s2 -> s2 ++ " " ++ (toString <| secPerms s2) )

--thrPermsShow : String -> String
--thrPermsShow = (\s3 -> s3 ++ " " ++ (toString <| thrPerms s3) )

--ansPermsShow : String -> String
--ansPermsShow = (\s4 -> s4 ++ " " ++ (toString <| ansPerms s4) )

-- innerShow = s1 ++ " " ++ (toString inner)
-- twoListPermsShow =            toString <| twoListPerms            inner s2
-- threeListPermsShow =          toString <| threeListPerms          inner s2 s3
-- answersPlusListShow =         toString <| answersPlusList         inner s2 s3
-- findSpecificAnswerShow =      toString <| findSpecificAnswer      inner s2 s3 <| ansPerms s4
-- answersPermsPlusListShow =    toString <| answersPermsPlusList    inner s2 s3
-- displaySpecificAnswersShow =  toString <| displaySpecificAnswers  inner s2 s3 answers


-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
updateModelLift : Signal Model
updateModelLift = Signal.foldp
                    updateModel
                    (
                      (0, "1,2,3", "4,5,6", "7,8,9", "12,15,18"),
                      (True),
                      ([1,2,3], [[4,5,6]], [[7,8,9]], [[12,15,18]], [[[2]]], [[[3]]],
                        ([([1], [[1]])], [([1], [[1]])], [([[1]], [[1]])], [([[1]], [[1]])]))
                    )
                    updatesChnl.signal

-- converts Update to new Model
updateModel : Update -> Model -> Model
updateModel update ( (i, s1, s2, s3, s4),
                     (b1),
                     --(xs, xxs2, xxs3, xxs4, s9, s10, (s11, s12, s13, s14))
                     results
                   ) =
  let
    createModel i s1 s2 s3 s4 b1 =
      let
        inner     = wheelPositionFromString s1
        answers   = wheelPositionFromString s4
        secLoop   = secPerms s2
        thrLoop   = thrPerms s3
        ansLoop   = ansPerms s4
      in
        ((i, s1, s2, s3, s4), (b1),
          (inner, secLoop, thrLoop, ansLoop,
            twoLoopPerms inner secLoop, threeLoopPerms inner secLoop thrLoop,
            (answersPlusList      inner secLoop thrLoop,
              findSpecificAnswer  inner secLoop thrLoop ansLoop,
              answersPermsPlusList inner secLoop thrLoop,
              displaySpecificAnswers inner secLoop thrLoop answers)))
  in
    case update of
      NoOp        ->    createModel  i      s1 s2 s3 s4 b1
      Add val     ->    createModel (i + 1) s1 s2 s3 s4 (not b1)
      Remove val  ->    createModel (i - 1) s1 s2 s3 s4 (True)

      UpdateField s ->  createModel  i      s  s2 s3 s4 (b1)
      Circle2Field s -> createModel  i      s1 s  s3 s4 (b1)
      Circle3Field s -> createModel  i      s1 s2 s  s4 (b1)
      Circle4Field s -> createModel  i      s1 s2 s3 s  (b1)

strToNum : String -> Int
strToNum s = resToNum (String.toInt s)

resToNum : Result e Int -> Int
resToNum r =
  case r of
    Ok x -> x
    Err s -> 0

--M
wheelPositionFromString : String -> WheelPosition
wheelPositionFromString s =
  List.map
    strToNum
      (String.split "," s)

-- PUZZLE SOLUTIONS

turnWheel : WheelPosition -> Int -> WheelPosition
turnWheel wheel turns = (drop turns wheel) ++ (take turns wheel)

getWheelLoop : List WheelPosition -> WheelPosition -> Int -> WheelLoop
getWheelLoop positions pos count =
  case count of
    0 ->
      --lists ++ [seed]
      [pos] ++ positions
    otherwise ->
      getWheelLoop ([turnWheel pos count] ++ positions) pos (count-1)

-- M
createWheelLoop : WheelPosition -> WheelLoop
createWheelLoop initialPos = getWheelLoop [] initialPos ( (length initialPos) - 1 )

-- M
generateWheelLoop : String -> WheelLoop
generateWheelLoop s = createWheelLoop <| wheelPositionFromString s

-- M
secPerms  = generateWheelLoop
thrPerms  = generateWheelLoop
ansPerms  = generateWheelLoop

-- M
twoLoopPerms : WheelPosition -> WheelLoop -> List LoopsPermutation
twoLoopPerms inner secLoop = List.map (\sec -> inner :: sec :: []) secLoop

appendTwoLoopPerms : WheelPosition -> WheelLoop -> WheelPosition -> List LoopsPermutation
appendTwoLoopPerms inner secLoop thrPermsItem =
  map (\xs ->  xs ++ [thrPermsItem]) (twoLoopPerms inner secLoop)

-- M
threeLoopPerms : WheelPosition -> WheelLoop -> WheelLoop -> List LoopsPermutation
threeLoopPerms inner secLoop thrLoop =
  concat <| map (appendTwoLoopPerms inner secLoop) thrLoop

sumColumn : LoopsPermColumn -> Int
sumColumn (a, b, c) = a + b + c

columnsFromLists : LoopsPermutation -> List LoopsPermColumn
columnsFromLists lists =
    let list1   = headLLI lists
        list2   = headLLI <| drop 1 lists
        list3   = headLLI <| drop 2 lists
    in
        zip3 list1 list2 list3

zip3 : WheelPosition -> WheelPosition -> WheelPosition -> List LoopsPermColumn
zip3 pos1 pos2 pos3 = List.map3 (,,) pos1 pos2 pos3

headLLI : LoopsPermutation -> WheelPosition
headLLI = foldr (\h t -> h) []

headLLIxx xs =
  let
    h = head xs
  in
    case h of
      Just x  -> x
      Nothing -> []


sumPlusLists : LoopsPermutation -> List (LoopsPermAnswer, LoopsPermutation)
sumPlusLists perm = [(map sumColumn <| columnsFromLists perm, perm)]

-- NOTE this refactors out two later steps by comparing list to loop of answers
-- M
answersPlusList : WheelPosition -> WheelLoop -> WheelLoop ->
                    List (LoopsPermAnswer, LoopsPermutation)
answersPlusList inner secLoop thrLoop =
  concat <| map sumPlusLists (threeLoopPerms inner secLoop thrLoop)

-- M
findSpecificAnswer : WheelPosition ->
                       WheelLoop -> WheelLoop -> WheelLoop ->
                         List (LoopsPermAnswer, LoopsPermutation)
findSpecificAnswer inner secLoop thrLoop answersLoop =
    filter (\(answer, lists) -> elem2 answer answersLoop)
                <| answersPlusList inner secLoop thrLoop

answersPermsLoop2 : (LoopsPermAnswer, t) -> (LoopsPermutation, t)
answersPermsLoop2 (ans, lists) = (createWheelLoop ans, lists)

answersPermsPlusList : WheelPosition -> WheelLoop -> WheelLoop ->
                        List (LoopsAnswerLoop, LoopsPermutation)
answersPermsPlusList inner secLoop thrLoop =
  map answersPermsLoop2 <| answersPlusList inner secLoop thrLoop

-- finds solution
findSpecificAnswerPlusList : WheelPosition -> WheelLoop -> WheelLoop -> WheelPosition ->
                              List (LoopsAnswerLoop, LoopsPermutation)
findSpecificAnswerPlusList inner secLoop thrLoop answers =
    filter (\(ans, lists) -> elem2 answers ans)
              <| answersPermsPlusList inner secLoop thrLoop

-- display solution
displaySpecificAnswers : WheelPosition -> WheelLoop -> WheelLoop -> WheelPosition ->
                          List (LoopsAnswerLoop, LoopsPermutation)
                          -- (LoopsAnswerLoop, LoopsPermutation) -- head
displaySpecificAnswers inner secLoop thrLoop answers =
  -- snd <|
  -- headX <|
  findSpecificAnswerPlusList inner secLoop thrLoop answers


--headX : List (LoopsAnswerLoop, LoopsPermutation) -> (LoopsAnswerLoop, LoopsPermutation)
headXY = foldr (\h t -> h) []
headX xs =
  let
    h = head xs
  in
    case h of
      Just x  -> x
      Nothing -> ([[0]], [[0]])

elem2 : LoopsPermAnswer -> LoopsAnswerLoop -> Bool
elem2 a (x::xs) =
  case (x == a) of
    True -> True
    False ->
      case (xs == []) of
        True -> False
        False -> elem2 a xs


-- haskell foldr map
