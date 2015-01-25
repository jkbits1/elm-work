import Graphics.Element (..)
import Signal (..)
import List (..)
import Text (..)


main : Element
main = asText (sqAll[1,2,3])
-- , , 

-- main = asText (product2 (drop 1 [1,2,3,4]))
-- main = asText (product (drop 1 [1,2,3,4]))
-- main = asText (head (drop 1 [1,2,3]))
-- main = plainText (head (drop 1 [1,2,3]))

-- product : List Integer -> Integer
-- product [] = 1
-- product (x:xs) = x * product xs

sqAll : List Int -> List Int
sqAll list = 
  case list of 
    []      -> []
    x :: xs -> sqr x :: sqAll xs
    
sqr : Int -> Int
sqr x = x * x

      
        
--    x :: xs ->
--     x * product xs
      
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
  