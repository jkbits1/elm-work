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

type alias Wheel            = List Int
type alias WheelLoop        = List (List Int) -- all turns of wheel
type alias LoopsPermutation = List (List Int) -- combo of one or more loops
type alias LoopsAnswer      = List Int        -- results of combo addition
type alias LoopsAnswerLoop = List Wheel      -- all turns of combo addition

-- values received from UI
type alias ModelInputs  = (Int, String, String, String, String)

-- values generated from UI input
type alias ModelResults =
  (Wheel, WheelLoop, WheelLoop, WheelLoop,
    List LoopsPermutation,                       List LoopsPermutation,
    (List (LoopsAnswer, LoopsPermutation),       List (LoopsAnswer, LoopsPermutation),
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
        inner     = wheelFromString s1
        answers   = wheelFromString s4
      in
        ((i, s1, s2, s3, s4), (b1),
          (inner, secPerms s2, thrPerms s3, ansPerms s4,
            twoListPerms inner s2, threeListPerms inner s2 s3,
            (answersPlusList inner s2 s3, findSpecificAnswer inner s2 s3 <| ansPerms s4,
              answersPermsPlusList inner s2 s3, displaySpecificAnswers inner s2 s3 answers)))
  in
    case update of
      NoOp        ->    createModel  i      s1 s2 s3 s4 b1
      Add val     ->    createModel (i + 1) s1 s2 s3 s4 (not b1)
      Remove val  ->    createModel (i - 1) s1 s2 s3 s4 (True)

      UpdateField s ->  createModel  i      s  s2 s3 s4 (b1)
      Circle2Field s -> createModel  i      s1 s  s3 s4 (b1)
      Circle3Field s -> createModel  i      s1 s2 s  s4 (b1)
      Circle4Field s -> createModel  i      s1 s2 s3 s  (b1)

wheelFromString : String -> Wheel
wheelFromString s =
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
      --lists ++ [seed]
      [seed] ++ lists
    otherwise ->
      listLoops ([listLoopItem seed count] ++ lists) seed (count-1)

wheelPerms : Wheel -> WheelLoop
wheelPerms xs = listLoops [] xs ( (length xs) - 1 )

secPerms : String -> WheelLoop
secPerms  = (\s2 -> wheelPerms <| wheelFromString s2)

thrPerms : String -> WheelLoop
thrPerms = (\s3 -> wheelPerms <| wheelFromString s3)

ansPerms : String -> WheelLoop
ansPerms = (\s4 -> wheelPerms <| wheelFromString s4)

twoListPerms : Wheel -> String -> List LoopsPermutation
twoListPerms inner s2 = List.map (\sec -> inner :: sec :: []) (secPerms s2)

appendTwoListPerms : Wheel -> String -> List Int -> List LoopsPermutation
appendTwoListPerms inner s2 thr = map (\xs ->  xs ++ [thr]) (twoListPerms inner s2)

threeListPerms : Wheel -> String -> String -> List LoopsPermutation
threeListPerms inner s2 s3 = concat <| map (appendTwoListPerms inner s2) (thrPerms s3)

sumTriple : (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

tuplesFromLists : List (List Int) -> List (Int, Int, Int)
tuplesFromLists lists =
    let list1   = headLLI lists
        list2   = headLLI <| drop 1 lists
        list3   = headLLI <| drop 2 lists
    in
        zip3 list1 list2 list3

zip3 : List Int -> List Int -> List Int -> List (Int, Int, Int)
zip3 l1 l2 l3 = List.map3 (,,) l1 l2 l3

headLLI : List (List Int) -> List Int
headLLI xs =
  let
    h = head xs
  in
    case h of
      Just x  -> x
      Nothing -> []

sumPlusLists : List (List Int) -> List (List Int, List (List Int))
sumPlusLists lists = [(map sumTriple <| tuplesFromLists lists, lists)]

-- NOTE this refactors out two later steps by comparing list to loop of answers
answersPlusList : Wheel -> String -> String -> List (LoopsAnswer, LoopsPermutation)
answersPlusList inner s2 s3 = concat <| map sumPlusLists (threeListPerms inner s2 s3)

findSpecificAnswer : Wheel -> String -> String ->
                              List (List Int) ->
                              List (LoopsAnswer, LoopsPermutation)
findSpecificAnswer inner s2 s3 answersPerms =
    filter (\(answer, lists) -> elem2 answer answersPerms) <| answersPlusList inner s2 s3

answersPermsLoop2 : (List Int, t) -> (List (List Int), t)
answersPermsLoop2 (ans, lists) = (wheelPerms ans, lists)

answersPermsPlusList : Wheel -> String -> String ->
                        List (LoopsAnswerLoop, LoopsPermutation)
answersPermsPlusList inner s2 s3 = map answersPermsLoop2 <| answersPlusList inner s2 s3

-- finds solution
findSpecificAnswerPlusList : Wheel -> String -> String -> Wheel ->
                              List (LoopsAnswerLoop, LoopsPermutation)
findSpecificAnswerPlusList inner s2 s3 answers =
    filter (\(ans, lists) -> elem2 answers ans) <| answersPermsPlusList inner s2 s3

-- display solution
displaySpecificAnswers : Wheel -> String -> String -> Wheel ->
                          List (List (List Int), List (List Int))
displaySpecificAnswers inner s2 s3 answers =
  -- snd <|
  -- headX <|
  findSpecificAnswerPlusList inner s2 s3 answers


headX : List (List (List Int), List (List Int)) -> (List (List Int), List (List Int))
headX xs =
  let
    h = head xs
  in
    case h of
      Just x  -> x
      Nothing -> ([[0]], [[0]])

elem2 : List Int -> List (List Int) -> Bool
elem2 a (x::xs) =
  case (x == a) of
    True -> True
    False ->
      case (xs == []) of
        True -> False
        False -> elem2 a xs


