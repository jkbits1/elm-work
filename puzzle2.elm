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

type alias ModelInputs = (Int, String, String, String, String)
type alias ModelResults =
  (String, String, String, String, String, String,
    (String, String, String, String))

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
  --, ("height", "70px")
  , ("padding", "10px 0")
  , ("font-size", "2em")
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
                          (s5, s6, s7, s8, s9, s10, (s11, s12, s13, s14))
                        ) =
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
    div [ style textStyle] [ text ("first xxxxx - " ++ s5) ],
    div [ style textStyle] [ text ("secPerms - " ++ s6) ],
    div [ style textStyle] [ text ("triPermsx - " ++ s7) ],
    div [ style textStyle] [ text ("ansPermsx - " ++ s8) ],
    div [ style textStyle] [ text ("2listPermsx - " ++ s9) ],
    div [ style textStyle] [ text ("3listPermsx - " ++ s10) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("answersPlus - " ++ s11) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("findAnswers - " ++ s12) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("answersPerms - " ++ s13) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("displayAnswer - " ++ s14) ]
  ]

-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
updateModelLift : Signal Model
updateModelLift = Signal.foldp
                    updateModel
                    (
                      (0, "1,2,3", "4,5,6", "7,8,9", "12,15,18"),
                      (True),
                      ("r1", "r2", "r3", "r4", "r5", "r6", ("r7", "r8", "r9", "r10"))
                    )
                    updatesChnl.signal

-- converts Update to new Model
updateModel : Update -> Model -> Model
updateModel update ( (i, s1, s2, s3, s4),
                     (b1),
                     (s5, s6, s7, s8, s9, s10, (s11, s12, s13, s14))
                   ) =
  let
    inner     = circleNumsFromString s1
    answers   = circleNumsFromString s4
    innerShow = s1 ++ " " ++ (toString inner)
    twoListPermsShow =            toString <| twoListPerms            inner s2
    threeListPermsShow =          toString <| threeListPerms          inner s2 s3
    answersPlusListShow =         toString <| answersPlusList         inner s2 s3
    findSpecificAnswerShow =      toString <| findSpecificAnswer      inner s2 s3 <| ansPerms s4
    answersPermsPlusListShow =    toString <| answersPermsPlusList    inner s2 s3
    displaySpecificAnswersShow =  toString <| displaySpecificAnswers  inner s2 s3 answers

    createModel i s1 s2 s3 s4 b1 =
                  ((i, s1, s2, s3, s4), (True),
                    (innerShow, secPermsShow s2, thrPermsShow s3, ansPermsShow s4,
                      twoListPermsShow, threeListPermsShow,
                      (answersPlusListShow, findSpecificAnswerShow,
                        answersPermsPlusListShow, displaySpecificAnswersShow)))
  in
    case update of
      NoOp        ->    createModel  i      s1 s2 s3 s4 b1
      Add val     ->    createModel (i + 1) s1 s2 s3 s4 (not b1)
      Remove val  ->    createModel (i - 1) s1 s2 s3 s4 (True)

      UpdateField s ->  createModel  i      s  s2 s3 s4 (True)
      Circle2Field s -> createModel  i      s1 s  s3 s4 (True)
      Circle3Field s -> createModel  i      s1 s2 s  s4 (True)
      Circle4Field s -> createModel  i      s1 s2 s3 s  (True)

circleNumsFromString : String -> List Int
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

secPerms : String -> List (List Int)
secPerms  = (\s2 -> wheelPerms <| circleNumsFromString s2)

thrPerms : String -> List (List Int)
thrPerms = (\s3 -> wheelPerms <| circleNumsFromString s3)

ansPerms : String -> List (List Int)
ansPerms = (\s4 -> wheelPerms <| circleNumsFromString s4)

secPermsShow : String -> String
secPermsShow  = (\s2 -> s2 ++ " " ++ (toString <| secPerms s2) )

thrPermsShow : String -> String
thrPermsShow = (\s3 -> s3 ++ " " ++ (toString <| thrPerms s3) )

ansPermsShow : String -> String
ansPermsShow = (\s4 -> s4 ++ " " ++ (toString <| ansPerms s4) )


twoListPerms : List Int -> String -> List (List (List Int ))
twoListPerms inner s2 = List.map (\sec -> inner :: sec :: []) (secPerms s2)

appendTwoListPerms : List Int -> String -> List Int -> List (List (List Int))
appendTwoListPerms inner s2 thr = map (\xs ->  xs ++ [thr]) (twoListPerms inner s2)

threeListPerms : List Int -> String -> String -> List (List (List Int))
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

-- NOTE can factor some later steps by comparing list to loop of answers
-- NOTE can factor some later steps by comparing list to loop of answers
answersPlusList : List Int -> String -> String -> List (List Int, List (List Int))
answersPlusList inner s2 s3 = concat <| map sumPlusLists (threeListPerms inner s2 s3)

findSpecificAnswer : List Int -> String -> String ->
                              List (List Int) ->
                              List (List Int, List (List Int))
findSpecificAnswer inner s2 s3 answersPerms =
    filter (\(answer, lists) -> elem2 answer answersPerms) <| answersPlusList inner s2 s3

answersPermsLoop2 : (List Int, t) -> (List (List Int), t)
answersPermsLoop2 (ans, lists) = (wheelPerms ans, lists)

answersPermsPlusList : List Int -> String -> String -> List ((List (List Int), List (List Int)))
answersPermsPlusList inner s2 s3 = map answersPermsLoop2 <| answersPlusList inner s2 s3

-- finds solution
findSpecificAnswerPlusList : List Int -> String -> String -> List Int ->
                              List (List (List Int), List (List Int))
findSpecificAnswerPlusList inner s2 s3 answers =
    filter (\(ans, lists) -> elem2 answers ans) <| answersPermsPlusList inner s2 s3

-- display solution
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


