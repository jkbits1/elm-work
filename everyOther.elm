import Graphics.Element (..)
import Signal (..)
--import List (..)
import List ( (::) )
--import List ( map )
import List 
import Text (..)

zipWith = List.map2
-- zipWith = List.length
-- zipWith = map

main : Element
--main = asText (List.filter filterEvenTuples (pairList['a', 'b', 'c']))
--main = asText (everyOther ['a', 'b', 'c'])
--main = asText (everyOther [1, 2, 3, 4])
main = asText (everyOther ["Tom", "Sue", "Sam"])
--main = asText (everyOther [])


--everyOther : List a -> List (a, Int)
everyOther : List a -> List (a)
everyOther list = 
  List.map pickFirstTupleItem (List.filter filterEvenTuples (pairList list))

pickFirstTupleItem : (a, b) -> a
pickFirstTupleItem (a, _) = a

isEven : Int -> Bool
isEven a = a % 2 == 0
  

filterEvenTuples : (a, Int) -> Bool
filterEvenTuples (a, b) = 
  case isEven b of 
    True -> False
    _    -> True

-- main = asText (product2 (drop 1 [1,2,3,4]))
-- main = asText (product (drop 1 [1,2,3,4]))
-- main = asText (head (drop 1 [1,2,3]))
-- main = plainText (head (drop 1 [1,2,3]))

-- product : List Integer -> Integer
-- product [] = 1
-- product (x:xs) = x * product xs

pairList : List a -> List (a, Int)
pairList list =
--  List.map2 (,) list [1..5000]
  zipWith (,) list [1..5000]

dropT : List String -> List String
dropT list = 
  case list of 
    []      -> []
    x :: xs -> if | x == "Tom"  -> xs
                  | otherwise   -> x :: dropT xs
    

      
        
      
-- product2 : List Int -> Int
-- product2 []         = 1
-- product2 (x :: xs)  = x * product2 xs

product2 : List Int -> Int
product2 list =
  case list of   
    []        -> 1
    (x :: xs) -> x * product2 xs

-- from vid, 20:31
-- http://elm-lang.org/learn/courses/beginner/Lists-and-Records.elm
-- sum numbers =
--   [] -> 0
--   hd :: tl -> hd + sum tl
  