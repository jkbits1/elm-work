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
    gameOver  : Bool,
    message   : String
  }
  
init : Model  
init = 
  {
    rowCount  = 1,
    cells     = ['_', '_', '_'],
    moves     = [],
    gameOver  = False,
    message   = ""
  }

view : Address Action -> Model -> Html
view address model =   
  div [class "row"]
  ( 
    boardAsHtml model ++
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
      then 
        let newCells     = processMove moveAsChar model.cells
            newMoves     = [action] ++ model.moves 
        in
        if checkForWin newCells
          then 
          {
            rowCount  = model.rowCount,
            cells     = newCells,
            moves     = newMoves,
            gameOver  = True,
            message   = "You win!" -- ++ (String.concat moves) 
--            message   = "You win!" ++ (joinMoves moves)
          } 
          else
          {
            rowCount  = model.rowCount,
            cells     = newCells,
            moves     = newMoves,
            gameOver  = model.gameOver,
            message   = model.message          
          }
      else model

boardAsHtml : Model -> List Html
boardAsHtml model = 
  let htmlOutput = [text (String.fromList model.cells) ] in
  if model.gameOver
    then htmlOutput ++
          [ br[][], text model.message ]       
    else htmlOutput 
--          ++ [text <| String.concat model.moves]

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
    
checkForWin : List Char -> Bool
checkForWin cs = 
    let testChar = 'X' in
        maybeHeadToBlank cs == testChar 
            && (maybeHeadToBlank <| List.drop 1 cs) == testChar 
            && (maybeHeadToBlank <| List.drop 2 cs) == testChar

--joinMoves : List String -> String
--joinMoves cs = List.foldr (++) "" cs
