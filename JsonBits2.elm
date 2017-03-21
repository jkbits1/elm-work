module JsonBits2 exposing (..)

import Http exposing (..)
import Json.Decode exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (Maybe)

type alias Model = {
    count : Int
  , firstFileName : String 
  , currentFileName : String
  , fileNames : List String
  , xvals : List Int
  , titleDetails : List TitleDetail
  , sortDetailsByLength : Bool
  , filter : Bool
  , showJsonErrors : Bool
  , filterLength : Float
  , httpInfo : String
  }

type alias TitleDetail =
  { 
    titleNumber : Int
  , length: Float
  }

type SortBy = SortByLength | SortByNumber

type Msg = 
    NoOp

  | ButtonGetFirstFileName
  | ButtonGetFileNames
  | ButtonGetFileDetails
  | ButtonGetFileDetailsWrapped

  | SortDetails SortBy

  | Filter
  | FilterLen String

  | ShowJsonErrors

  | CurrentFileName String

  | InfoFirstFileName (Result Http.Error String)
  | InfoFileNames (Result Http.Error (List String))
  | InfoFileNamesMaybe (Result Http.Error (List (Maybe String)))
  | InfoTitleDetails (Result Http.Error (List TitleDetail))

-- Html Event for select onChange
onChange : (String -> msg) -> Attribute msg
onChange f = on "change" <| Json.Decode.map f Html.Events.targetValue

onChangeSort : Attribute Msg
onChangeSort = 
  onChange 
    (\val ->  
      ( case val of 
          "length"  -> SortDetails SortByLength
          _         -> SortDetails SortByNumber ))

onChangeFileName : Attribute Msg
onChangeFileName = onChange (\s -> CurrentFileName s)          

checkbox : Msg -> String -> Html.Html Msg
checkbox msg name =
  label
    [ 
      -- style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", onClick msg ] []
    , Html.text name
    ]

infoListItems : List String -> List (Html Msg)
infoListItems xs = 
  List.map (\s -> li [] [text <| s]) xs

detailsDisplay : TitleDetail -> String
detailsDisplay td = 
  (String.padRight 20 '.' <| toString td.titleNumber) ++ " " ++ 
    (toString td.length)

detailsListItems : List TitleDetail -> List (Html Msg)
detailsListItems tds = 
  List.map (\td -> li [] [text <| detailsDisplay td]) tds

vidInfoStubURL : String
vidInfoStubURL = "http://localhost:8000/vidInfo"

vidInfoFilesURL : String
vidInfoFilesURL = "http://localhost:8000/vidInfo/files" 

vidInfoURLWrapped : String
vidInfoURLWrapped = "http://localhost:8000/vidInfoWrapped"

-- not used
titleDetailsList3a : Json.Decode.Decoder (String)
titleDetailsList3a =
  (field "titleDetails" Json.Decode.string) 
    |> andThen 
    -- sort of a NoOp 
             (\s ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <| s
             )

titleDetailDecoder : Decoder TitleDetail
titleDetailDecoder = map2 TitleDetail (field "titleNumber" int) (field "length" float)

titleDetailsList : Json.Decode.Decoder (List TitleDetail)
titleDetailsList =
  (field "titleDetails" string) |> 
    andThen 
      (\s ->
        decodeString (Json.Decode.list titleDetailDecoder) s |>
          (\result ->
            case result of 
              Ok xs -> Json.Decode.succeed xs
              Err err -> Json.Decode.fail <| err ++ " fn: titleDetailsList "
          )
      )

getFileDetailsReqWrapped : String -> Http.Request (List TitleDetail)
getFileDetailsReqWrapped string =
  Http.get 
    (vidInfoURLWrapped ++ "/" ++ string)
    titleDetailsListWrapped

httpSendTitleDetails : Request (List TitleDetail) -> Cmd Msg
httpSendTitleDetails = Http.send InfoTitleDetails

getFileDetailsWrapped : String -> Cmd Msg
getFileDetailsWrapped string = 
  httpSendTitleDetails <| getFileDetailsReqWrapped string

getFileDetails : String -> Cmd Msg
getFileDetails string = 
  httpSendTitleDetails <| getFileDetailsReq string

titleDetailsListWrapped : Json.Decode.Decoder (List TitleDetail)
titleDetailsListWrapped =
  Json.Decode.at ["wrapper", "titleDetails"] <| 
    Json.Decode.list <|
      titleDetailDecoder

getFileDetailsReq : String -> Http.Request (List TitleDetail)
getFileDetailsReq string =
  Http.get 
    (vidInfoStubURL ++ "\\" ++ string)
    -- titleDetailsListOrig
    titleDetailsList

--  customDecoder decoder toResult = 
--    Json.Decode.andThen
--            (\a ->
--                  case toResult a of 
--                     Ok b -> Json.Decode.succeed b
--                     Err err -> Json.Decode.fail err
--            )
--            decoder

getFirstStringx : List String -> Json.Decode.Decoder String
getFirstStringx strings =

-- ODDLY, this line below worked, with the anon fn below
-- all taken from customDecoder code
  -- stringList |> andThen 

-- customDecoder decoder toResult = 
  --  Json.Decode.andThen
            --  (\xs ->
                -- create Decoder String that decodes to first file name
                -- succeed : a -> Decoder a
                Json.Decode.succeed <|
                  Maybe.withDefault "" <| List.head strings --xs 
            --  )
            --  decoder


getFirstString : Decoder String
getFirstString = 
  stringListDecoder |> andThen 
             (\xs ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <|
                  Maybe.withDefault "" <| List.head xs 
             )

stringListDecoder : Json.Decode.Decoder (List String)    
stringListDecoder = Json.Decode.list Json.Decode.string

maybeStringList : Json.Decode.Decoder (List (Maybe.Maybe String))    
maybeStringList = Json.Decode.list (nullable Json.Decode.string)
-- decodeString (list (nullable string)) """["42", null, "43"]"""
-- decodeString maybeStringListx """["42", null, "43"]"""

getFirstFileName : String -> Cmd Msg
getFirstFileName string = 
  Http.send InfoFirstFileName <|
    Http.get 
      vidInfoFilesURL 
      getFirstString

getFileNames : String -> Cmd Msg
getFileNames string = Http.send InfoFileNames <| getFileNamesReq

getFileNamesReq : Http.Request (List String)
getFileNamesReq = Http.get vidInfoFilesURL stringListDecoder

getFileNamesMaybe : String -> Cmd Msg
getFileNamesMaybe string = Http.send InfoFileNamesMaybe <| getFileNamesMaybeReq

getFileNamesMaybeReq : Http.Request (List (Maybe String))
getFileNamesMaybeReq = Http.get vidInfoFilesURL maybeStringList

getFirstFileNameTest : String -> Cmd Msg
getFirstFileNameTest string = 
  Http.send InfoFirstFileName <|
    Http.get 
      ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
      getFirstStringTest

getFirstStringTest : Json.Decode.Decoder String
getFirstStringTest =
  Json.Decode.at ["data", "image_url"] Json.Decode.string

-- getFileNamesAsStringReq : String -> Http.Request String
-- getFileNamesAsStringReq string = Http.getString vidInfoFilesURL        

-- getFileNamesAsStringCmd : Cmd Msg
-- getFileNamesAsStringCmd = Http.send Info <| getFileNamesAsStringReq ""

