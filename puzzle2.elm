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
type alias LoopsPermutation = List WheelPosition -- item from combo of one or more loops
type alias LoopsPermColumn  = (Int, Int, Int)  -- values from a single loops perm
type alias LoopsPermAnswers = List Int        -- results of combo addition
type alias LoopsAnswerLoop  = List WheelPosition      -- all positions of combo addition
type alias Counter          = List Int

-- values received from UI
type alias ModelInputs  = (Int, String, String, String, String)

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
                          (firstList, secLoop, thrList, ansList, twoListPerms, threeListPerms,
                            (ansPlusList, specificAnswer, ansPermsPlusList, specificAnswerPlusList
                              , findAnswerLazy3))
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
    div [ style textStyle] [ text ("first  - " ++ (toString firstList)) ],
    div [ style textStyle] [ text ("secLoop - " ++ (toString secLoop)) ],
    div [ style textStyle] [ text ("thrLoop - " ++ (toString thrList)) ],
    div [ style textStyle] [ text ("ansLoop - " ++ (toString ansList)) ],
    div [ style textStyle] [ text ("2loopPerms - " ++ (toString twoListPerms)) ],
    div [ style textStyle] [ text ("3loopPerms - " ++ (toString threeListPerms)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("answersPlus - " ++ (toString ansPlusList)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("findAnswers - " ++ (toString specificAnswer)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("answersPerms - " ++ (toString ansPermsPlusList)) ],
    div [ style <| textStyle ++ (displayStyle b1)] [ text ("displayAnswer - " ++ (toString specificAnswerPlusList)) ]
    , div [ style <| textStyle ++ (displayStyle b1)] [ text ("lazyAnswer - " ++ (toString findAnswerLazy3)) ]
  ]

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
-- answersPlusListShow =         toString <| answersPlusList         first s2 s3
-- findSpecificAnswerShow =      toString <| findSpecificAnswer      first s2 s3 <| getAnsLoop s4
-- answersPermsPlusListShow =    toString <| answersPermsPlusList    first s2 s3
-- displaySpecificAnswersShow =  toString <| displaySpecificAnswers  first s2 s3 answers


-- converts Signal Update (from updatesChnl) to Signal Model, 
-- using non-signal updateModel
updateModelLift : Signal Model
updateModelLift = Signal.foldp
                    updateModel
                    (
                      (0, "1,2,3", "4,5,6", "7,8,9", "12,15,18"),
                      --(0, "6,5,5,6,5,4,5,4", "4,2,2,2,4,3,3,1", "1,3,2,3,3,2,4,3",
                        --    "12,8,12,10,10,12,10,8"),
                      (True),
                      ([1,2,3], [[4,5,6]], [[7,8,9]], [[12,15,18]], [[[2]]], [[[3]]],
                        ([([1], [[1]])], [([1], [[1]])], [([[1]], [[1]])], [([[1]], [[1]])]
                        ,([1], [[1]])
                        )
                      )
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
        first     = wheelPositionFromString s1
        answers   = wheelPositionFromString s4
        secLoop   = makeSecLoop s2
        thrLoop   = makeThrLoop s3
        ansLoop   = makeAnsLoop s4
      in
        ((i, s1, s2, s3, s4), (b1),
          (first, secLoop, thrLoop, ansLoop,
            twoWheelPerms first secLoop, threeLoopPerms first secLoop thrLoop,
            (answersPlusList      first secLoop thrLoop,
              findSpecificAnswer  first secLoop thrLoop ansLoop,
              answersPermsPlusList first secLoop thrLoop,
              displaySpecificAnswers first secLoop thrLoop answers
              , findAnswerLazy3 first secLoop thrLoop ansLoop)))
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
makeSecLoop  = generateWheelLoop
makeThrLoop  = generateWheelLoop
makeAnsLoop  = generateWheelLoop

-- M
twoWheelPerms : WheelPosition -> WheelLoop -> List LoopsPermutation
twoWheelPerms first secLoop = map (\secPosition -> first :: secPosition :: []) secLoop

appendTwoWheelPerms : List LoopsPermutation -> WheelPosition -> List LoopsPermutation
appendTwoWheelPerms twoWheelPermsLocal thrPos =
  map (\twoLoopsPerm -> twoLoopsPerm ++ [thrPos]) twoWheelPermsLocal

-- M
threeLoopPerms : WheelPosition -> WheelLoop -> WheelLoop -> List LoopsPermutation
threeLoopPerms first secLoop thrLoop =
  let
    addPosToTwoWheelPerms = appendTwoWheelPerms <| twoWheelPerms first secLoop
  in
    concat <| map addPosToTwoWheelPerms thrLoop

sumColumn : LoopsPermColumn -> Int
sumColumn (a, b, c) = a + b + c

getSpecificPos : Int -> LoopsPermutation -> WheelPosition
getSpecificPos pos = headLLI << drop pos

columnsFromPermutation : LoopsPermutation -> List LoopsPermColumn
columnsFromPermutation perm =
    let
      firstPos  = getSpecificPos 0 perm
      secPos    = headLLI <| drop 1 perm
      thrPos    = headLLI <| drop 2 perm
    in
        zip3 firstPos secPos thrPos

zip3 : WheelPosition -> WheelPosition -> WheelPosition -> List LoopsPermColumn
zip3 pos1 pos2 pos3 = map3 (,,) pos1 pos2 pos3

-- headLLI : LoopsPermutation -> WheelPosition
headLLI : List WheelPosition -> WheelPosition
headLLI = foldr (\h t -> h) []

headLLIxx xs =
  let
    h = head xs
  in
    case h of
      Just x  -> x
      Nothing -> []


sumPlusPerm : LoopsPermutation -> List (LoopsPermAnswers, LoopsPermutation)
sumPlusPerm perm = [(map sumColumn <| columnsFromPermutation perm, perm)]

-- NOTE this refactors out two later steps by comparing list to loop of answers
-- M
answersPlusList : WheelPosition -> WheelLoop -> WheelLoop ->
                    List (LoopsPermAnswers, LoopsPermutation)
answersPlusList first secLoop thrLoop =
  concat <| map sumPlusPerm <| threeLoopPerms first secLoop thrLoop

-- M
findSpecificAnswer : WheelPosition ->
                       WheelLoop -> WheelLoop -> WheelLoop ->
                         List (LoopsPermAnswers, LoopsPermutation)
findSpecificAnswer first secLoop thrLoop answersLoop =
    filter (\(answer, _) -> elem2 answer answersLoop)
                <| answersPlusList first secLoop thrLoop

answersPermsLoop2 : (LoopsPermAnswers, t) -> (LoopsPermutation, t)
answersPermsLoop2 (ans, lists) = (createWheelLoop ans, lists)

answersPermsPlusList : WheelPosition -> WheelLoop -> WheelLoop ->
                        List (LoopsAnswerLoop, LoopsPermutation)
answersPermsPlusList first secLoop thrLoop =
  map answersPermsLoop2 <| answersPlusList first secLoop thrLoop

-- finds solution
findSpecificAnswerPlusList : WheelPosition -> WheelLoop -> WheelLoop -> WheelPosition ->
                              List (LoopsAnswerLoop, LoopsPermutation)
findSpecificAnswerPlusList first secLoop thrLoop answers =
    filter (\(ans, lists) -> elem2 answers ans)
              <| answersPermsPlusList first secLoop thrLoop

-- display solution
displaySpecificAnswers : WheelPosition -> WheelLoop -> WheelLoop -> WheelPosition ->
                          List (LoopsAnswerLoop, LoopsPermutation)
                          -- (LoopsAnswerLoop, LoopsPermutation) -- head
displaySpecificAnswers first secLoop thrLoop answers =
  -- snd <|
  -- headX <|
  (\h -> [h]) <| headX <|
  findSpecificAnswerPlusList first secLoop thrLoop answers


-- headXY : List (LoopsAnswerLoop, LoopsPermutation) -> (LoopsAnswerLoop, LoopsPermutation)
--headXY : List (a, b) -> (a, b)
--headXY = foldr (\h t -> h) ()

headX : List (LoopsAnswerLoop, LoopsPermutation) -> (LoopsAnswerLoop, LoopsPermutation)
headX xs =
  let
    h = head xs
  in
    case h of
      Just x  -> x
      Nothing -> ([[0]], [[0]])

elem2 : LoopsPermAnswers -> LoopsAnswerLoop -> Bool
elem2 a answerLoop =
  case answerLoop of
    (x::xs) ->
      case (x == a) of
        True -> True
        False ->
          case (xs == []) of
            True -> False
            False -> elem2 a xs
    [] -> False


-- haskell foldr map


-- Lazy solution

quotRem : Int -> Int -> (Int, Int)
quotRem a b =
  let
    quot = (//) a b
  in
    (quot, rem a b)

digitsRev base n =
  case n of
    0 -> []
    _ ->
      let
        (rest, lastDigit) = quotRem n base
      in
        lastDigit :: digitsRev base rest

digits base = (reverse) << (digitsRev base)

getCounter : Int -> (Int, Counter)
getCounter x =
  case x >= 512 of
    True  -> getCounter 0
    False ->
      let
        xs = digits 8 x
      in
        case length xs of
          0 -> (x, [0,0,0])
          1 -> (x, [0,0] ++ xs)
          2 -> (x, [0] ++ xs)
          otherwise -> (x, xs)

wheelPermsItem : Int -> WheelLoop -> WheelPosition
wheelPermsItem idx loop = headLLI <| drop idx loop

headCounter : List Int -> Int
headCounter = foldr (\h t -> h) 0

threeWheelsPermsItemByCounter : WheelPosition -> WheelLoop -> WheelLoop ->
                                  (a, Counter) -> LoopsPermutation
threeWheelsPermsItemByCounter first secLoop thrLoop (_, counter) =
  let
    sec_idx = headCounter counter
    thr_idx = headCounter <| drop 1 counter
  in
    [first]
    ++
    [wheelPermsItem sec_idx secLoop] ++
    [wheelPermsItem thr_idx thrLoop]

wheelsTuple : LoopsPermutation -> List LoopsPermColumn
wheelsTuple xxs =
  let
    inn = getSpecificPos 0 xxs
    sec = getSpecificPos 1 xxs
    thr = getSpecificPos 2 xxs
  in
    zip3 inn sec thr

getWheelsPermAnswers : WheelPosition -> WheelLoop -> WheelLoop -> Int ->
                        LoopsPermAnswers
getWheelsPermAnswers first secLoop thrLoop n =
  map sumColumn <| wheelsTuple <|
        threeWheelsPermsItemByCounter first secLoop thrLoop
        <| getCounter n

findAnswerLazy3 : WheelPosition -> WheelLoop -> WheelLoop -> WheelLoop ->
                    (LoopsPermAnswers, LoopsPermutation)
findAnswerLazy3 first secLoop thrLoop ansLoop =
  let
    ansIdx = headCounter <|
      map (\(i, _) -> i) <|
        filter (\(i, b) -> b == True) <|
          map (\i -> (i, elem2 (getWheelsPermAnswers first secLoop thrLoop i) ansLoop))
            -- [1..5000000] - elm can't cope, as it doesn't do lazy eval
            [1..512]
    answers = getWheelsPermAnswers first secLoop thrLoop ansIdx
    loopsPerm = threeWheelsPermsItemByCounter first secLoop thrLoop <| getCounter ansIdx
  in
    (answers, loopsPerm)

initCounter : Counter
initCounter = [0, 0, 0]

