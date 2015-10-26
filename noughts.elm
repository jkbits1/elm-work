import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String

-- converts Signal Model to Signal Html, using non-signal view
main : Signal Html
main = StartApp.start { model = model, view = view, update = update }

model : Int
model = 0

-- view : Address String -> Int -> Html
view : Address Action -> Int -> Html
view address model =   
  div [class "row"]
  ( row model ++
    [
      div [class "move"]
      [
        input
          [
            placeholder "Move"
            , value (toString model)
            , on "input" targetValue (Signal.message address)
          ]
          []
      ]
    ]
  )
  
  
row : Int -> List Html
row model = 
  -- text "X":: text "X" :: text "X" :: []
  if model == 0 
    then [text "X", text "_", text "_"]
    else [text "_", text "_", text "_"]
    

--processMove move cs =
--    if move == '0'
--        then 'X': (drop 1 cs)
--        else if move == '1'
--            then (head cs): 'X': (drop 2 cs)
--            else (take 2 cs) ++ ['X']

type alias Action = String  

update : Action -> Int -> Int
update action model =
  case action of
    "0" -> 0
    otherwise -> 1

