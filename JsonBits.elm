module JsonBits exposing (..)

-- import List exposing (..)
import Json.Decode exposing (..)
import Http exposing (..)

type alias Model = {
    count : Int
  , info : String 
  }


type Msg = 
    NoOp
  | ButtonGet
  | ButtonGetFirstFileName
  | ButtonGetFileNames
  | Info (Result Http.Error String)
  | InfoFirstFileName (Result Http.Error String)
  | InfoList (Result Http.Error (List String))

vidInfoFilesURL : String
vidInfoFilesURL =
  -- url 
    -- "http://localhost:9090/vidInfo/files" 
    "http://localhost:8000/vidInfo/files" 
    -- []

-- decodeString (list string) "[\"test1\", \"test2\"]"

-- getFirstString : List String -> Task Http.Error String
-- getFirstString strings =
getFirstStringx : List String -> Json.Decode.Decoder String
getFirstStringx strings =
-- getFirstString =

-- ODDLY, this line below worked, with the anon fn below
-- all taken from customDecoder code
  -- stringList |> andThen 

-- customDecoder decoder toResult = 
  --  Json.Decode.andThen
            --  (\xs ->
                -- create Decoder String that decodes to first file name
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
  stringList |> andThen getFirstStringx
  
stringList : Json.Decode.Decoder (List String)    
stringList = Json.Decode.list Json.Decode.string

-- getFirstFileName : String -> Task Http.Error String
-- getFirstFileName string = 
--   Http.get stringList
--     vidInfoFilesURL `andThen` getFirstString

getFirstFileName : String -> Cmd Msg
getFirstFileName string = 
  Http.send InfoFirstFileName <|
    Http.get 
      -- ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
    -- stringList
      vidInfoFilesURL 
      -- stringList
      -- <| andThen getFirstString
      getFirstString

-- 
-- NOTE: most direct translation from 017 code so far
-- 
--  Http.get JsonBits.vidInfoFilesURL JsonBits.stringList
-- Request { method = "GET", headers = [], url = "http://localhost:8000/vidInfo/files", body = EmptyBody, expect = { responseType = "text", responseToResult = <function> }, timeout = Nothing, withCredentials = False }
--     : Http.Request (List String)

-- getFileNames : String -> Task Http.Error (List String)
getFileNames : String -> Cmd Msg
getFileNames string = 
  Http.send InfoList <|
    getFileNamesReq

getFileNamesReq : Http.Request (List String)
getFileNamesReq =
    Http.get 
      vidInfoFilesURL 
      stringList
--   Http.get stringList
--     vidInfoFilesURL `andThen` getStrings

-- so far, it seems this is no longer needed in 0180
-- getStrings : List String -> Task Http.Error (List String)
-- getStrings strings =
--   case strings of
--     string :: _ -> succeed strings
--     [] ->
--       fail (Http.UnexpectedPayload "expecting 1 or more strings from server")

-- getFirstFileNameNew : String -> Task Http.Error String
getFirstFileNameNew string = 
  Http.send InfoFirstFileName <|
    Http.get 
      ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
      getFirstStringNew
    -- stringList
    -- vidInfoFilesURL 
    -- `andThen` getFirstString

getFirstStringNew : Json.Decode.Decoder String
getFirstStringNew =
  Json.Decode.at ["data", "image_url"] Json.Decode.string


  -- decodeString (list string) "[\"test1\", \"test2\"]"

-- see parse.html for testing this data
-- {"data":{"type":"gif","id":"3gTiFkr0fgt9K","url":"http:\/\/giphy.com\/gifs\/mom-almost-copy-3gTiFkr0fgt9K","image_original_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_mp4_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.mp4","image_frames":"51","image_width":"600","image_height":"723","fixed_height_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200_d.gif","fixed_height_downsampled_width":"166","fixed_height_downsampled_height":"200","fixed_width_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200w_d.gif","fixed_width_downsampled_width":"200","fixed_width_downsampled_height":"241","fixed_height_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100.gif","fixed_height_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100_s.gif","fixed_height_small_width":"83","fixed_height_small_height":"100","fixed_width_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w.gif","fixed_width_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w_s.gif","fixed_width_small_width":"100","fixed_width_small_height":"121","username":"","caption":""},"meta":{"status":200,"msg":"OK","response_id":"588193312fdd614bd4b29d28"}}


-- getFileNamesAsString : String -> Task Http.Error String
getFileNamesAsString : String -> Http.Request String
getFileNamesAsString string = 
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
  Http.send Info <| getFileNamesAsString ""

--getFileNamesAsString string = getStringCors "http://localhost:9090/vidInfo/files"
