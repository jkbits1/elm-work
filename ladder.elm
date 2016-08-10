import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as HtmlApp
import List exposing (..)

type alias Player = 
    {   name:   String
    ,   id:     Int
    }

type alias Model = 
    {
        players : List Player
    ,   newName : String
    }    

type Msg = NoOp | Add | NewName String

init = ({players = [], newName = ""}, Cmd.none)

main = HtmlApp.program { init = init, view = view, update = update, subscriptions = subscriptions}

view : Model -> Html Msg
view model = div [] [
        div [] [
            text "player details"
        ]
    ,   div [] [
            input [onInput NewName
            -- , id "inputPlayer"
            ]
            []
        ,   button [
                onClick Add
            ] [text "Add player"]
        ]
    ,   createPlayersHtml model.players

  ] 

createPlayersHtml : List Player -> Html Msg
createPlayersHtml model =   
    ul [] -- [
        -- li [] [
        --     text "player item"
        -- ]
        <| List.map liFromPlayer model
    -- ]
        
liFromPlayer : Player -> Html Msg
liFromPlayer player =
    li [] [
        text player.name    
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        NoOp ->         (model, Cmd.none)
        NewName s ->    ({players = model.players, newName = s}, Cmd.none)
        Add ->          (
            {   
                players = 
                    [{name = model.newName, id = (length model.players) + 1}] ++ model.players
            ,   newName = model.newName
            }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
