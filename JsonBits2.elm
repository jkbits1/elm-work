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
  , applyFilter : Bool
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

  | GetFirstFileName
  | GetFileNames
  | GetFileDetails
  | GetFileDetailsWrapped

  | SortDetails SortBy

  | Filter
  | FilterLen String

  | ShowJsonErrors

  | GetCurrentFileNameDetails String

  | InfoFirstFileName (Result Http.Error String)
  | InfoFirstFileNameMaybe (Result Http.Error (Maybe String))

  | InfoFileNames (Result Http.Error (List String))
  | InfoFileNamesMaybe (Result Http.Error (List (Maybe String)))

  | InfoTitleDetails (Result Http.Error (List TitleDetail))
  | InfoTitleDetailsMaybe (Result Http.Error (List (Maybe TitleDetail)))


-- VIEW HELPERS

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
onChangeFileName = onChange (\s -> GetCurrentFileNameDetails s)          

checkbox : Msg -> Bool -> String -> Html.Html Msg
checkbox msg checkedState name =
  label
    [ 
      -- style [("padding", "20px")]
    ]
    [ input [ type_ "checkbox", checked checkedState, onClick msg ] []
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


-- DECODERs

stringListDecoder : Json.Decode.Decoder (List String)    
stringListDecoder = Json.Decode.list Json.Decode.string

firstStringDecoder : Decoder String
firstStringDecoder = 
  stringListDecoder |> andThen 
             (\xs ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <|
                  Maybe.withDefault "" <| List.head xs 
             )

maybeStringListDecoder : Json.Decode.Decoder (List (Maybe String))    
maybeStringListDecoder = Json.Decode.list (nullable Json.Decode.string)
-- decodeString (list (nullable string)) """["42", null, "43"]"""
-- decodeString maybeStringListx """["42", null, "43"]"""

maybeFirstStringDecoder : Decoder (Maybe String)
maybeFirstStringDecoder = 
  maybeStringListDecoder |> andThen 
             (\xs ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <| 
                  Maybe.withDefault Nothing <| List.head xs
             )

titleDetailDecoder : Decoder TitleDetail
titleDetailDecoder = map2 TitleDetail (field "titleNumber" int) (field "length" float)

titleDetailListDecoder : Json.Decode.Decoder (List TitleDetail)
titleDetailListDecoder =
  (field "titleDetails" string) |> 
    andThen 
      (\s ->
        decodeString (Json.Decode.list titleDetailDecoder) s |>
          (\result ->
            case result of 
              Ok xs -> Json.Decode.succeed xs
              Err err -> Json.Decode.fail <| err ++ " fn: titleDetailListDecoder "
          )
      )

titleDetailListMaybeDecoder : Json.Decode.Decoder (List (Maybe TitleDetail))
titleDetailListMaybeDecoder =
  -- this first check (for nullable string) could be used in non-maybe decoder, too
  (field "titleDetails" (nullable string)) |> 
    andThen 
      (\m ->
        let 
          s = 
            case m of 
              Just str -> str
              Nothing -> ""
        in
          decodeString (Json.Decode.list (nullable titleDetailDecoder)) s |>
            (\result ->
              case result of 
                Ok xs -> Json.Decode.succeed xs
                Err err -> Json.Decode.fail <| err ++ " fn: titleDetailListMaybeDecoder "
            )
      )

titleDetailListWrapped : Json.Decode.Decoder (List TitleDetail)
titleDetailListWrapped =
  Json.Decode.at ["wrapper", "titleDetails"] <| 
    Json.Decode.list <|
      titleDetailDecoder

--  customDecoder decoder toResult = 
--    Json.Decode.andThen
--            (\a ->
--                  case toResult a of 
--                     Ok b -> Json.Decode.succeed b
--                     Err err -> Json.Decode.fail err
--            )
--            decoder


-- URLs

serverAddress     = "http://localhost:8000/"

createVidInfoURL : String -> String
createVidInfoURL  = (++) serverAddress

vidInfoFilesURL   = createVidInfoURL "vidInfo/files" 
vidInfoStubURL    = createVidInfoURL "vidInfo"
vidInfoURLWrapped = createVidInfoURL "vidInfoWrapped"


-- HTTP.REQUESTs

getFirstFileNameReq : Http.Request String
getFirstFileNameReq = 
  Http.get vidInfoFilesURL firstStringDecoder

getFirstFileNameMaybeReq : Http.Request (Maybe String)
getFirstFileNameMaybeReq = 
  Http.get vidInfoFilesURL maybeFirstStringDecoder

getFileNamesReq :                     Http.Request (List String)
getFileNamesReq = 
  Http.get vidInfoFilesURL stringListDecoder

getFileNamesMaybeReq :                Http.Request (List (Maybe String))
getFileNamesMaybeReq = 
  Http.get vidInfoFilesURL maybeStringListDecoder

getFileDetailsWrappedReq :  String -> Http.Request (List TitleDetail)
getFileDetailsWrappedReq string =
  Http.get 
    (vidInfoURLWrapped ++ "/" ++ string)
    titleDetailListWrapped

getFileDetailsReq :         String -> Http.Request (List TitleDetail)
getFileDetailsReq string =
  Http.get (vidInfoStubURL ++ "\\" ++ string) titleDetailListDecoder

getFileDetailsMaybeReq :         String -> Http.Request (List (Maybe TitleDetail))
getFileDetailsMaybeReq string =
  Http.get (vidInfoStubURL ++ "\\" ++ string) titleDetailListMaybeDecoder


-- CMDs

getFirstFileNameCmd :       String -> Cmd Msg
getFirstFileNameCmd string = 
  Http.send InfoFirstFileName       <| getFirstFileNameReq

getFirstFileNameMaybeCmd :  String -> Cmd Msg
getFirstFileNameMaybeCmd string = 
  Http.send InfoFirstFileNameMaybe  <| getFirstFileNameMaybeReq

getFileNamesCmd :           String -> Cmd Msg
getFileNamesCmd string = 
  Http.send InfoFileNames           <| getFileNamesReq

getFileNamesMaybeCmd :      String -> Cmd Msg
getFileNamesMaybeCmd string = 
  Http.send InfoFileNamesMaybe      <| getFileNamesMaybeReq

httpSendTitleDetailsCmd : Request (List TitleDetail) -> Cmd Msg
httpSendTitleDetailsCmd = 
  Http.send InfoTitleDetails

getFileDetailsWrappedCmd :  String -> Cmd Msg
getFileDetailsWrappedCmd string = 
  httpSendTitleDetailsCmd           <| getFileDetailsWrappedReq string

getFileDetailsCmd :         String -> Cmd Msg
getFileDetailsCmd string = 
  httpSendTitleDetailsCmd           <| getFileDetailsReq string

getFileDetailsMaybeCmd :         String -> Cmd Msg
getFileDetailsMaybeCmd string = 
  Http.send InfoTitleDetailsMaybe   <| getFileDetailsMaybeReq string

