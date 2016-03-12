-- NOTE: code comes from Signal.send package example
-- have changed fn names to show lifting fns (not sure if it is clearer)

--  <link rel="stylesheet" href="css/tidy.css">

module PuzzleModule where

import Signal

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
answersPlusPerm : WheelPosition -> WheelLoop -> WheelLoop ->
                    List (LoopsPermAnswers, LoopsPermutation)
answersPlusPerm first secLoop thrLoop =
  concat <| map sumPlusPerm <| threeLoopPerms first secLoop thrLoop

-- M
findSpecificAnswer : WheelPosition ->
                       WheelLoop -> WheelLoop -> WheelLoop ->
                         List (LoopsPermAnswers, LoopsPermutation)
findSpecificAnswer first secLoop thrLoop answersLoop =
    filter (\(answer, _) -> elem2 answer answersLoop)
                <| answersPlusPerm first secLoop thrLoop

answersPermsLoop2 : (LoopsPermAnswers, t) -> (LoopsPermutation, t)
answersPermsLoop2 (ans, lists) = (createWheelLoop ans, lists)

answersPermsPlusList : WheelPosition -> WheelLoop -> WheelLoop ->
                        List (LoopsAnswerLoop, LoopsPermutation)
answersPermsPlusList first secLoop thrLoop =
  map answersPermsLoop2 <| answersPlusPerm first secLoop thrLoop

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

