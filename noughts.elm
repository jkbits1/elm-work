import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String

main : Signal Html
main = StartApp.start { model = init, view = view, update = update }

type alias Action = String  

-- this stores game Board and moves together = 
type alias Model = 
  { cells : List Char,
    moves : List String
  }
  
init : Model  
init = 
  {
    cells = ['_', '_', '_'],
    moves = []
  }

view : Address Action -> Model -> Html
view address model =   
  div [class "row"]
  ( 
    row model ++
    [
      div [class "move"]
      [
        input
          [
            placeholder "Move"
--            , value (toString (List.head model.moves))
            , on "input" targetValue (Signal.message address)
          ]
          []
      ]
    ]
  )
    
row : Model -> List Html
row model = [text (String.fromList model.cells) ]

maybeToBlank : Maybe Char -> Char
maybeToBlank maybeChar = 
  case maybeChar of 
    Just value  -> value
    Nothing     -> '_'

processMove : Char -> List Char -> List Char
processMove move cs =
  case move of 
    '0' -> 'X' :: (List.drop 1 cs)
    '1' -> maybeToBlank (List.head cs) :: 'X' :: (List.drop 2 cs)
    otherwise -> (List.take 2 cs) ++ ['X']
    
validateMove : Char -> Bool
validateMove move =
  if move == '0' || move == '1' || move == '2' 
    then True
    else False
 
update : Action -> Model -> Model
update action model =
    let moveAsChar = (maybeToBlank (List.head (String.toList action)))
        valid = validateMove moveAsChar 
    in
    if valid 
      then { 
        cells = processMove moveAsChar model.cells,
        moves = [action] ++ model.moves
      } 
      else model
