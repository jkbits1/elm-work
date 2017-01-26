module JsonBits2 exposing (..)

-- import List exposing (..)
import Json.Decode exposing (..)
import Http exposing (..)

type alias Model = {
    count : Int
  , info : String 
  , fileNames : List String
  , xvals : List Int
  , titleDetails : List TitleDetail
  }

type alias TitleDetail =
  { 
    titleNumber : Int
  , length: Float
  }

type Msg = 
    NoOp
  | ButtonGet
  | ButtonGetFirstFileName
  | ButtonGetFileNames
  | ButtonGetFileDetails
  | ButtonGetFileDetails3
  | ButtonGetFileDetailsWrapped
  | Info (Result Http.Error String)
  | InfoFirstFileName (Result Http.Error String)
  | InfoList (Result Http.Error (List String))
  | InfoFileDetails1 (Result Http.Error (List String))
  -- | InfoFileDetails (Result Http.Error String)
  | InfoFileDetails2 (Result Http.Error (List Int))
  | InfoFileDetails (Result Http.Error (List TitleDetail))

vidInfoFilesURL : String
vidInfoFilesURL =
  -- url 
  -- "http://localhost:9090/vidInfo/files" 
  "http://localhost:8000/vidInfo/files" 
  -- ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
  -- []

vidInfoURL : String
vidInfoURL =
 "http://localhost:8000/vidInfo"
--  Http.url "http://localhost:9090/vidInfo" []
--   Http.url "http://localhost:9090/vidInfoWrapped" []

vidInfoURLWrapped : String
vidInfoURLWrapped =
  "http://localhost:8000/vidInfoWrapped"


    -- titleDetailsDecoder : Json.DecodeDecoder (List String)    
-- titleDetailsDecoder = 
-- --  Debug.log "titleDetails" <|
--     "titleDetails" |> field Json.Decodelist Json.Decodestring
    
-- getFileDetailsReq : String -> Http.Request String
getFileDetailsReq : String -> Http.Request (List String)
-- getFileDetailsReq : String -> Http.Request (List TitleDetail)
getFileDetailsReq string =
  Http.get 
    (vidInfoURL ++ "\\" ++ string)
    titleDetailsList2
    -- andThen
    --     getTitleDetails

-- original version
-- getFileDetails : String -> Task Http.Error (List String)
-- getFileDetails string = 
--   Http.get 
--     titleDetailsList
--       (vidInfoURL ++ "\\" ++ string)
--       -- (vidInfoURL)
--       `andThen`
--         getTitleDetails

getFileDetails : String -> Cmd Msg
getFileDetails string = 
  Http.send InfoFileDetails1 <|
    getFileDetailsReq string

getFileDetailsReq2 : String -> Http.Request (List Int)
getFileDetailsReq2 string =
  Http.get 
    (vidInfoURL ++ "\\" ++ string)
    titleDetailsList4

getFileDetails2 : String -> Cmd Msg
getFileDetails2 string = 
  Http.send InfoFileDetails2 <|
    getFileDetailsReq2 string

getFileDetailsReq3 : String -> Http.Request (List TitleDetail)
getFileDetailsReq3 string =
  Http.get 
    (vidInfoURL ++ "\\" ++ string)
    -- titleDetailsListOrig
    titleDetailsList

getFileDetails3 : String -> Cmd Msg
getFileDetails3 string = 
  Http.send InfoFileDetails <|
    getFileDetailsReq3 string

-- this was required for Signals. It created a task from
-- the list of data that was used as an input to a Signal.
-- getTitleDetails : List TitleDetail -> Task Http.Error (List String)
-- getTitleDetails details =
--   case details of
--     string :: _ -> succeed 
--       (List.map (toString) details)
-- --      [
-- ----      Debug.log "files dets" "file details"
-- --        "dummy title dets"
-- --      ]
--     [] ->
-- --      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")
--         succeed ["no details found"]

-- this was required for Signals. It created a task from
-- the list of data that was used as an input to a Signal.
-- getTitleSpecifics : List TitleDetail -> Task Http.Error (List TitleDetail)
-- getTitleSpecifics details =
--   case details of
--     detail :: _ -> succeed 
--       details
--     [] ->
-- --      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")
--         succeed 
--           [TitleDetail 0 0.0]

titleDetailsList2 : Json.Decode.Decoder (List String)
titleDetailsList2 =
  field "titleDetails" <| Json.Decode.list Json.Decode.string

titleDetailsList3 : Json.Decode.Decoder (String)
titleDetailsList3 =
  field "titleDetails" Json.Decode.string

titleDetailsList3a : Json.Decode.Decoder (String)
titleDetailsList3a =
  (field "titleDetails" Json.Decode.string) 
    |> andThen 
    -- sort of a NoOp 
             (\s ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <| s
             )

titleDetailsList4 : Json.Decode.Decoder (List Int)
titleDetailsList4 =
  (field "titleDetails" string) |> 
    andThen 
      (\s ->
        decodeString (list (field "titleNumber" int)) s |>
          (\r ->
            case r of 
              Ok xs -> Json.Decode.succeed xs
              Err err -> Json.Decode.fail <| err ++ "xx"
          )
      )

-- original code from earlier Elm version, adjusted as field params reversed.
-- No longer works, seems to need intermediate string conversion now
titleDetailsListOrig : Json.Decode.Decoder (List TitleDetail)
titleDetailsListOrig =
  (field "titleDetails" 
    (list <|
      Json.Decode.map2 TitleDetail
        (field "titleNumber" int)
        (field "length" float)
    )
  )

-- original code
--titleDetailsList : Json.DecodeDecoder (List TitleDetail)
--titleDetailsList =
--  ("titleDetails" |> field (Json.Decodelist <|
--      Json.Decodemap2 TitleDetail
--        ("titleNumber" |> field Json.Decodeint)
--        ("length" |> field Json.Decodefloat)
--    )
--  )


titleDetailDecoder : Decoder TitleDetail
titleDetailDecoder = map2 TitleDetail (field "titleNumber" int) (field "length" float)

titleDetailsList : Json.Decode.Decoder (List TitleDetail)
titleDetailsList =
  (field "titleDetails" string) |> 
    andThen 
      (\s ->
        decodeString (list titleDetailDecoder) s |>
          (\result ->
            case result of 
              Ok xs -> Json.Decode.succeed xs
              Err err -> Json.Decode.fail <| err ++ "xx"
          )
      )


-- original code for wrapped details api
-- getFileSpecifics : String -> Task Http.Error (List TitleDetail)
-- getFileSpecifics string = 
--   Http.get
--     titleDetailsList
-- --      (vidInfoURL ++ "\\" ++ string)
--       (vidInfoURL)
--         `andThen` getTitleSpecifics

getFileDetailsReqWrapped : String -> Http.Request (List TitleDetail)
getFileDetailsReqWrapped string =
  Http.get 
    (vidInfoURLWrapped ++ "\\" ++ string)
    titleDetailsListWrapped

getFileDetailsWrapped : String -> Cmd Msg
getFileDetailsWrapped string = 
  Http.send InfoFileDetails <|
    getFileDetailsReqWrapped string


titleDetailsListWrapped : Json.Decode.Decoder (List TitleDetail)
titleDetailsListWrapped =
  Json.Decode.at ["wrapper", "titleDetails"] <| 
    Json.Decode.list <|
      -- Json.Decode.map2 TitleDetail
      --   (field "titleNumber" Json.Decode.int)
      --   (field "length" Json.Decode.float)
      -- this function contains the map2 code above
      titleDetailDecoder

-- original code for wrapped api
-- titleDetailsList : Json.DecodeDecoder (List TitleDetail)
-- titleDetailsList =
--   Json.Decode.at ["wrapper", "titleDetails"] <| 
--     Json.Decode.list <|
--       Json.Decode.object2 TitleDetail
-- --        ("titleNumber" := Json.string)
-- --        ("length" := Json.string)
--         ("titleNumber" := Json.Decode.int)
--         ("length" := Json.Decode.float)
      


-- decodeString : Decoder a -> String -> Result String a
-- succeed : a -> Decoder a

--  customDecoder decoder toResult = 
--    Json.Decode.andThen
--            (\a ->
--                  case toResult a of 
--                     Ok b -> Json.Decode.succeed b
--                     Err err -> Json.Decode.fail err
--            )
--            decoder

-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[1,2]\"}"""
-- Ok "[1,2]" : Result.Result String String
-- decodeString (list (field "titleNumber" int)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"
-- Ok [1] : Result.Result String (List Int)


-- titleDetailsList3b : Json.Decode.Decoder (String)
-- titleDetailsList3b =
--   (field "titleDetails" 
--     Json.Decode.string <| Json.Decode.string
--   ) 

-- titleDetailsList3c : Json.Decode.Decoder (String)
-- titleDetailsList3c =
--   (field "titleDetails" 
--     <| Json.Decode.list Json.Decode.string
--   )


-- from elm packages page, works in elm repl
-- decodeString (list string) "[\"test1\", \"test2\"]"

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


  -- Json.Decode.decodeString (list Json.Decodestring) strings
  -- case strings of
  --   string :: _ -> succeed string
  --   [] ->
  --     fail (Http.UnexpectedPayload "expecting 1 or more strings from server")




-- (List String -> Decoder String) -> Decoder (List String) -> Decoder String

getFirstString : Decoder String
getFirstString = 
  stringList |> andThen 
    -- getFirstStringx
    -- incorporated code from getFirstStringx as anon fn
             (\xs ->
                -- create Decoder String that decodes to first file name
                Json.Decode.succeed <|
                  Maybe.withDefault "" <| List.head xs 
             )
            --  decoder
  
stringList : Json.Decode.Decoder (List String)    
stringList = Json.Decode.list Json.Decode.string

-- this was required for Signals. It created a task from
-- the list of data that was used as an input to a Signal.
-- getFirstString : List String -> Task Http.Error String
-- getFirstString strings =
--   case strings of
--     string :: _ -> succeed string
--     [] ->
--       fail (Http.UnexpectedPayload "expecting 1 or more strings from server")


-- original version
-- getFirstFileName : String -> Task Http.Error String
-- getFirstFileName string = 
--   Http.get stringList
--     vidInfoFilesURL `andThen` getFirstString

getFirstFileName : String -> Cmd Msg
getFirstFileName string = 
  Http.send InfoFirstFileName <|
    Http.get 
    -- stringList
      vidInfoFilesURL 
      -- <| andThen getFirstString
      getFirstString

-- 
-- NOTE: most direct translation from 017 code so far
-- 
--  Http.get JsonBits.vidInfoFilesURL JsonBits.stringList
-- Request { method = "GET", headers = [], url = "http://localhost:8000/vidInfo/files", body = EmptyBody, expect = { responseType = "text", responseToResult = <function> }, timeout = Nothing, withCredentials = False }
--     : Http.Request (List String)

-- original version
-- getFileNames : String -> Task Http.Error (List String)
-- getFileNames string = 
--   Http.get stringList
--     vidInfoFilesURL `andThen` getStrings

getFileNames : String -> Cmd Msg
getFileNames string = 
  Http.send InfoList <|
    getFileNamesReq

getFileNamesReq : Http.Request (List String)
getFileNamesReq =
    Http.get 
      vidInfoFilesURL 
      stringList

-- this was required for Signals. It created a task from
-- the list of data that was used as an input to a Signal.
-- getStrings : List String -> Task Http.Error (List String)
-- getStrings strings =
--   case strings of
--     string :: _ -> succeed strings
--     [] ->
--       fail (Http.UnexpectedPayload "expecting 1 or more strings from server")

getFirstFileNameTest : String -> Cmd Msg
getFirstFileNameTest string = 
  Http.send InfoFirstFileName <|
    Http.get 
      ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
      getFirstStringTest

getFirstStringTest : Json.Decode.Decoder String
getFirstStringTest =
  Json.Decode.at ["data", "image_url"] Json.Decode.string


  -- decodeString (list string) "[\"test1\", \"test2\"]"

-- see parse.html for testing this data
-- {"data":{"type":"gif","id":"3gTiFkr0fgt9K","url":"http:\/\/giphy.com\/gifs\/mom-almost-copy-3gTiFkr0fgt9K","image_original_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_mp4_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.mp4","image_frames":"51","image_width":"600","image_height":"723","fixed_height_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200_d.gif","fixed_height_downsampled_width":"166","fixed_height_downsampled_height":"200","fixed_width_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200w_d.gif","fixed_width_downsampled_width":"200","fixed_width_downsampled_height":"241","fixed_height_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100.gif","fixed_height_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100_s.gif","fixed_height_small_width":"83","fixed_height_small_height":"100","fixed_width_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w.gif","fixed_width_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w_s.gif","fixed_width_small_width":"100","fixed_width_small_height":"121","username":"","caption":""},"meta":{"status":200,"msg":"OK","response_id":"588193312fdd614bd4b29d28"}}

-- original version
-- getFileNamesAsString : String -> Task Http.Error String
-- getFileNamesAsString string = 
--   Http.getString vidInfoFilesURL        

-- getFileNamesAsString : String -> Task Http.Error String
getFileNamesAsStringReq : String -> Http.Request String
getFileNamesAsStringReq string = 
  Http.getString 
    vidInfoFilesURL        
    -- Http.url 
    -- "http://localhost:9090/vidInfo/files" 
    -- "http://localhost:8000/vidInfo/files" 
    -- "https://www.google.co.uk" 
    -- <| "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten"
    -- []

getFileNamesAsStringCmd : Cmd Msg
getFileNamesAsStringCmd = 
  Http.send Info <| getFileNamesAsStringReq ""

--getFileNamesAsString string = getStringCors "http://localhost:9090/vidInfo/files"

-- 
-- various experiments in Elm repl, to get json parsing correct
-- 
-- "{\"fileName\":\"red-info.txt\",\"titleDetails\":\"[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]\"}"

-- 
-- {"fileName":"red-info.txt","titleDetails":

-- decodeString JsonBits.titleDetailsList3a "{\"fileName\":\"red-info.txt\",\"titleDetails\":\"[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]\"}"

-- decodeString JsonBits.titleDetailsList3a "{\"titleDetails\":\"[{\"line\":\"ID_DVD_TITLE_1\",\"titleNumber\":1,\"length\":0.48}]\"}"

-- decodeString JsonBits.titleDetailsList3a "{"fileName":"red-info.txt","titleDetails":"[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"}"

-- can get a string value from a simple object
-- decodeString (field "fileName" string) """{ "fileName":"red-info.txt" }"""
-- Ok "red-info.txt" : Result.Result String String

-- so, it seems I can get a string from the field
-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[1,2]\"}"""
-- Ok "[1,2]" : Result.Result String String

-- and, given a string of a list, can get an actual list
-- decodeString (list int) "[1,2]"
-- Ok [1,2] : Result.Result String (List Int)

-- can get an empty list
-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[]\"}"""
-- Ok "[]" : Result.Result String String

-- and can get test list as decoded on its own (seem to have done this step earlier, too)
-- decodeString (field "titleDetails" string) """{\"titleDetails\":\"[1,2]\"}"""
-- Ok "[1,2]" : Result.Result String String

-- from the actual list, can get a single int
-- decodeString (list (field "titleNumber" int)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"
-- Ok [1] : Result.Result String (List Int)

-- from the actual list, can get a single string
-- decodeString (list (field "line" string)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}]"
-- Ok ["ID_DVD_TITLE_1_LENGTH=0.480"] : Result.Result String (List String)

-- from the actual list, can get multiple strings
-- decodeString (list (field "line" string)) "[{\"line\":\"ID_DVD_TITLE_1_LENGTH=0.480\",\"titleNumber\":1,\"length\":0.48}, {\"line\":\"ID_DVD_TITLE_1_LENGTH=0.481\",\"titleNumber\":2,\"length\":0.481}]"
-- Ok ["ID_DVD_TITLE_1_LENGTH=0.480","ID_DVD_TITLE_1_LENGTH=0.481"]
--     : Result.Result String (List String)


