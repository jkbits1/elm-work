import Html exposing (..)
import Html.App as HtmlApp

type alias Player = (String, Int)
type alias Model = List Player

type Msg = NoOp

init = ([], Cmd.none)

main = HtmlApp.program { init =init, view = view, update = update, subscriptions = subscriptions}

view : Model -> Html Msg
view model = div [] [
    div [] [
        text "player details"
    ]
  ] 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        NoOp -> ([], Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none