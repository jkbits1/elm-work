import Html exposing (Html, Attribute, div, text, input, br)
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
  { rowCount  : Int,
    cells     : List Char,
    moves     : List String,
    message   : String
  }
  
init : Model  
init = 
  {
    rowCount  = 1,
    cells     = ['_', '_', '_'],
    moves     = [],
    message   = ""
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

update : Action -> Model -> Model
update action model =
    let moveAsChar = (maybeToBlank (List.head (String.toList action)))
        valid = validateMove moveAsChar 
    in
    if valid 
      then {
        rowCount  = model.rowCount,
        cells     = processMove moveAsChar model.cells,
        moves     = [action] ++ model.moves,
        message   = model.message
      } 
      else model

row : Model -> List Html
row model = 
  let htmlOutput = [text (String.fromList model.cells) ] in
  if checkForWin model
    then [text "You win!", br[][]] ++ 
      htmlOutput
    else htmlOutput  

maybeToBlank : Maybe Char -> Char
maybeToBlank maybeChar = 
  case maybeChar of 
    Just value  -> value
    Nothing     -> '_'
    
maybeHeadToBlank : List Char -> Char
maybeHeadToBlank cs = maybeToBlank <| List.head cs

processMove : Char -> List Char -> List Char
processMove move cs =
  case move of 
    '0' -> 'X' :: (List.drop 1 cs)
    '1' -> maybeHeadToBlank cs :: 'X' :: (List.drop 2 cs)
    otherwise -> (List.take 2 cs) ++ ['X']
    
validateMove : Char -> Bool
validateMove move =
  if move == '0' || move == '1' || move == '2' 
    then True
    else False
    
checkForWin : Model -> Bool
checkForWin model = 
    let cs = model.cells in
    let testChar = 'X' in
        maybeHeadToBlank cs == testChar 
            && (maybeHeadToBlank <| List.drop 1 cs) == testChar 
            && (maybeHeadToBlank <| List.drop 2 cs) == testChar
             