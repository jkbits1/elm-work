import Html exposing (..)
import Html.Events exposing (..)
-- import Html.Attributes exposing (..)
import Html.App as HtmlApp
import List exposing (..)

type alias Player = 
    {   
        id:     Int
    ,   name:   String
    ,   ranking: Int
    }

type alias Model = 
    {
        players : List Player
    ,   newName : String
    }    

type Msg = NoOp | Add | NewName String

init : (Model, Cmd Msg)
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
    ,   playersHtml model.players

  ] 

playersHtml : List Player -> Html Msg
playersHtml model =   
    ul [] <| List.map playerLi model
        
playerLi : Player -> Html Msg
playerLi player =
    li [] [
        text <| player.name ++ " id: " ++ (toString player.id) ++ " r: " ++ (toString player.ranking)    
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        NoOp ->         (model, Cmd.none)
        NewName s ->    ({model | newName = s}, Cmd.none)
        Add ->          
            let 
                idNum = (length model.players) + 1
            in
                (
                    {  
                        model
                    |   players = 
                            [
                                {  id       = idNum
                                ,  name     = model.newName
                                ,  ranking  = idNum
                                }
                            ] ++ model.players  
                    }
                ,   Cmd.none
                )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
