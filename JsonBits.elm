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
  | ButtonGet2
  | ButtonGetFiles
  | Info (Result Http.Error String)
  | Info2 (Result Http.Error String)
  | InfoList (Result Http.Error (List String))

vidInfoFilesURL : String
vidInfoFilesURL =
  -- url 
    -- "http://localhost:9090/vidInfo/files" 
    "http://localhost:8000/vidInfo/files" 
    -- []


-- getFirstString : List String -> Task Http.Error String
-- getFirstString : List String -> Json.DecodeDecoder String
-- getFirstString strings =
-- getFirstString : Json.Decode.Decoder (List String)
getFirstString =
  Json.Decode.list Json.Decode.string
  -- Json.DecodedecodeString (list Json.Decodestring) strings
  -- case strings of
  --   string :: _ -> succeed string
  --   [] ->
  --     fail (Http.UnexpectedPayload "expecting 1 or more strings from server")

  -- decodeString (list string) "[\"test1\", \"test2\"]"




-- getFirstFileNameNew : String -> Task Http.Error String
getFirstFileNameNew string = 
  Http.send Info2 <|
    Http.get 
      ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
      getFirstStringNew
    -- stringList
    -- vidInfoFilesURL 
    -- `andThen` getFirstString

getFirstStringNew : Json.Decode.Decoder String
getFirstStringNew =
  Json.Decode.at ["data", "image_url"] Json.Decode.string

-- getFirstFileName : String -> Task Http.Error String
-- getFirstFileName : String -> Cmd Msg
getFirstFileName string = 
  Http.send InfoList <|
    Http.get 
      -- ("https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ "kitten")
    -- stringList
      vidInfoFilesURL 
    -- `andThen` getFirstString
      getFirstString


  -- decodeString (list string) "[\"test1\", \"test2\"]"

-- see parse.html for testing this data
-- {"data":{"type":"gif","id":"3gTiFkr0fgt9K","url":"http:\/\/giphy.com\/gifs\/mom-almost-copy-3gTiFkr0fgt9K","image_original_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.gif","image_mp4_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/giphy.mp4","image_frames":"51","image_width":"600","image_height":"723","fixed_height_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200_d.gif","fixed_height_downsampled_width":"166","fixed_height_downsampled_height":"200","fixed_width_downsampled_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/200w_d.gif","fixed_width_downsampled_width":"200","fixed_width_downsampled_height":"241","fixed_height_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100.gif","fixed_height_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100_s.gif","fixed_height_small_width":"83","fixed_height_small_height":"100","fixed_width_small_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w.gif","fixed_width_small_still_url":"http:\/\/media4.giphy.com\/media\/3gTiFkr0fgt9K\/100w_s.gif","fixed_width_small_width":"100","fixed_width_small_height":"121","username":"","caption":""},"meta":{"status":200,"msg":"OK","response_id":"588193312fdd614bd4b29d28"}}

