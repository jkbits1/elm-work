import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
--import StartApp.Simple as StartApp

--main = StartApp.start { model = model, view = view, update = update }
main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

--model : Int
--model = 0
init : (Int, Cmd Msg)
init = (0, Cmd.none)

subscriptions : Int -> Sub Msg
subscriptions model = Sub.none

--view : Signal.Address Action -> Int -> Html.Html
view : Int -> Html Msg
--view address model =
view model =
  div []
--    [ button [ onClick address Decrement ][ text "-" ]
    [ button [ onClick Decrement ][ text "-" ]
    , div [] [text (toString model)]
--    , button [onClick address Increment][ text "+"]
    , button [onClick Increment][ text "+"]
    ]
    
--type Action = Increment | Decrement
type Msg = Increment | Decrement

--update : Action -> Int -> Int
update : Msg -> Int -> (Int, Cmd Msg)
update action model =
  case action of 
    Increment -> (model + 1, Cmd.none)
    Decrement -> (model - 1, Cmd.none)
