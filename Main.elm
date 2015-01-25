module Main where

import Graphics.Element(Element)
import Text(..)
import Signal(..)

main:Signal Element
-- main = constant(asText "Hi Elm")
-- main = constant(asText "Hi Elm2")
-- main = constant(asText 42)
-- main = constant(asText [1,2,3])

main = constant(asText (square 5))

square n = n*n
