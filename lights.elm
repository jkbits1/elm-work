import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)

import LightsModule exposing (..)

main = Html.beginnerProgram
         { model = model, view = view, update = update }

type alias Model = (Int, ColourSet) 

model : Model
model = (1, stop)

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg (num, _) =
  let 
    nextNum = num + 1
    prevNum = num + 1
  in
    case msg of
      Increment -> (nextNum, safeStateByNum nextNum)
      Decrement -> (prevNum, safeStateByNum prevNum)

view : Model -> Html Msg
view (_, colourSet) =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , button [ onClick Increment ] [ text "+" ]
    , text <| toString colourSet
    ]
