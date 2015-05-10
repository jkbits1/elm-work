module Main where

import Graphics.Element exposing (Element, show)
--import Text(..)
import Signal exposing (constant)

main:Signal Element
-- main = constant(asText "Hi Elm")
-- main = constant(asText "Hi Elm2")
-- main = constant(asText 42)
-- main = constant(asText [1,2,3])

--main = constant(asText (square 5))
main = constant(show (square 5))

square n = n*n
