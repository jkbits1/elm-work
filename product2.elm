--import Graphics.Element (..)
--import Signal (..)
--import List (..)
--import Text (..)
import Html exposing (text)

--main : Element
main = text <| toString ([product [1,2,3] == 6, product [4,4] == 16, product [] == 1])
--main = asText ([product [1,2,3] == 6, product [4,4] == 16, product [] == 1])
-- , ,

-- main = asText (product2 (drop 1 [1,2,3,4]))
-- main = asText (product (drop 1 [1,2,3,4]))
-- main = asText (head (drop 1 [1,2,3]))
-- main = plainText (head (drop 1 [1,2,3]))

-- product : List Integer -> Integer
-- product [] = 1
-- product (x:xs) = x * product xs

product : List Int -> Int
product list = 
  case list of 
    [] -> 1
        
    x :: xs ->
      x * product xs
      
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
  