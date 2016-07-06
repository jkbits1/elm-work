port module TestInterop exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)

import List exposing (..)
import String

main = Html.program
  {
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =   
  {
    wheelDataList : List WheelData,
    dataProcessedItems : List String 
  }

initialModel : Model
initialModel =
  {
    wheelDataList = [wheelData1, wheelData2, wheelData3],
    dataProcessedItems = []
  }

init : (Model, Cmd Msg)
init = ( initialModel, Cmd.none)

type Msg =
  Change String
  | Check
  | Suggest (List String)

-- outgoing port to js
port check : List (List WheelItem) -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Change data -> 
      (initialModel, Cmd.none)

    Check ->
      (model, check model.wheelDataList)

    Suggest responses ->
      (
        newModelFromResponses model responses
        , Cmd.none)

newModelFromResponses model rs =
  let newWheelData1 =
    (createNewWheelStrings (headWD model.wheelDataList)
                    <| createFullListFromResponses
                        (headWD model.wheelDataList) rs)
  in
    {
      wheelDataList =
        [newWheelData1] ++ (drop 1 model.wheelDataList)
    , dataProcessedItems = []
    }

-- subscriptions, data responses from js
port dataProcessedItems : (List String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = dataProcessedItems Suggest
    
view : Model -> Html Msg
view model = div [] 
  [ 
    input [ onInput Change ] []
  , button [ onClick Check ] [ text "check"]
  , div [] [ text <| String.join ", "
        <| map .name <| headWD model.wheelDataList
      ]
  ]
  