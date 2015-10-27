import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String

-- converts Signal Model to Signal Html, using non-signal view
main : Signal Html
--main = StartApp.start { model = model, view = view, update = update }
main = StartApp.start { model = init, view = view, update = update }

--type alias Board = 
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

-- view : Address String -> Int -> Html
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
row model = 
  -- text "X":: text "X" :: text "X" :: []
--  case List.head model.moves of
--    Just move ->   
--      let cells = processMove 
--                    (maybeToBlank (List.head (String.toList move)))
--                    model.cells 
--      in
--      List.map (\x -> text (toString x)) cells
--      [text (toString cells) ]
      [text (String.fromList model.cells) ]
--      [text "X", text "_", text "_"]
--    otherwise ->  [text "_", text "_", text "_"]

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

type alias Action = String  

update : Action -> Model -> Model
update action model =
--  case action of
--    "0" ->        
    let cells = processMove 
                    (maybeToBlank (List.head (String.toList action)))
                    model.cells 
    in
    { 
      cells = cells,
--      cells = ['X', '_', '_'],
      moves = [action] ++ model.moves
    } 
--    otherwise ->  { 
--      cells = ['_', '_', '_'],
--      moves = model.moves
--    }

