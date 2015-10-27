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
  { cells : List (String),
    moves : List (String)
  }
  
init : Model  
init = 
  {
    cells = ["_", "_", "_"],
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
  if List.head model.moves == Just "0" 
    then [text "X", text "_", text "_"]
    else [text "_", text "_", text "_"]
    

--processMove move cs =
--    if move == '0'
--        then 'X': (drop 1 cs)
--        else if move == '1'
--            then (head cs): 'X': (drop 2 cs)
--            else (take 2 cs) ++ ['X']

type alias Action = String  

update : Action -> Model -> Model
update action model =
  case action of
    "0" ->        { 
      cells = ["X", "_", "_"],
      moves = [action] ++ model.moves
    } 
    otherwise ->  { 
      cells = ["_", "_", "_"],
      moves = model.moves
    }

