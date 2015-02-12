import Http
import Signal (constant, map)
import Text (asText)

addr = 
  \a -> constant a

main = 
  map asText (Http.sendGet (addr "http://localhost:3030/foldersDb"))