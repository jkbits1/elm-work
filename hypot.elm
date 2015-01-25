import Text(..)

hypot : Float -> Float -> Float
hypot a b = 
  sqrt((a^2) + (b^2))

main = asText [hypot 3 4, hypot 5 12, hypot 8 15]

