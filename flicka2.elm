-- experimenting with renaming fns

import Signal
import Graphics.Input.Field as Field
import Graphics.Element exposing (flow, down, Element, leftAligned)
--import Text
import Text exposing (fromString)
import String 
import Http
import Json.Decode exposing (decodeString, oneOf, int, string, customDecoder, at, list, object2, object3, (:=), Decoder)

-- channel that creates a signal for user-inputted tags
tagChnl : Signal.Mailbox Field.Content
tagChnl = Signal.mailbox Field.noContent

dbgChnl : Signal.Mailbox String
dbgChnl = Signal.mailbox ""

main : Signal Element
main = 
--  Signal.map2 
  Signal.map3 
    view (tagChnl.signal) results (dbgChnl.signal)

--view : Field.Content -> Maybe String -> Element
view : Field.Content -> Maybe String -> String -> Element
view searchContent imgSrc dbgInfo =
  let imgSrcDetails =
    case imgSrc of
      Just s -> s
      Nothing -> "no img"
  in
    flow down 
      [
          Field.field 
          Field.defaultStyle (Signal.send tagChnl) "search" searchContent
        , leftAligned <| Text.fromString imgSrcDetails --"123" 
        , leftAligned <| Text.fromString dbgInfo
      ]

results : Signal (Maybe String)
results =
--  NOTE: map with lamda needed for testing, converts String to Maybe String
--  Signal.map (\s -> Just s) <| 
--    Signal.constant "abc"
--    resultsInput <| 
--    resultsPhotoReqTest <| resultsInput
--    Signal.map toSizeRequestURL (resultsSendReq <| resultsPhotoReq resultsInput)
    Signal.map 
        toPhotoSource
--      NOTE: brackets ( or <| ) are important
        <| 
          respSignal 
          <|
            Signal.map
              convertRespToReq 
                (respSignal <| searchReqSignal tagSignal)
              
--resultsInput : Signal String
--resultsInput = 
tagSignal : Signal String
tagSignal = 
  Signal.dropRepeats <|
  Signal.map .string (tagChnl.signal)

resultsPhotoReqTest : Signal String -> Signal String
resultsPhotoReqTest input =
  Signal.map createPhotoSearchURL input
  
--resultsPhotoReq : Signal String -> Signal (Http.Request)
--resultsPhotoReq input =
searchReqSignal : Signal String -> Signal (Http.Request)
searchReqSignal tags =
  Signal.map createPhotoSearchRequest tags
  
--resultsSendReq : Signal (Http.Request String) -> Signal (Http.Response String)
--resultsSendReq : Signal (Http.Request) -> Signal (Http.Response)
--resultsSendReq req =
respSignal : Signal (Http.Request) -> Signal (Http.Response)
respSignal reqs =
  Http.send reqs
  
-- from http response, get "source" of first item (or return Nothing)
--toPhotoSource : Http.Response String -> Maybe String
toPhotoSource : Http.Response -> Maybe String
toPhotoSource json = 
  case decodeResponse sizeList json of
    Err msg ->
      Nothing
    
    Ok sizes ->
      case sizes of
        size :: _ -> Just size.source
        [] -> Nothing

--decodeResponse : Decoder a -> Http.Response String -> Result String a
decodeResponse : Decoder a -> Http.Response -> Result String a
decodeResponse decoder response =
--  case response of 
--    Http.Success str ->
--      decodeString decoder str
      decodeString decoder response.value
    
--    _ -> Err (toString response)
    
type alias Size = { source: String, width: Int, height: Int }    

sizeList : Decoder (List Size)
sizeList =
  let number =
    oneOf [int, customDecoder string String.toInt]
  in
    at ["sizes", "size"] <| list <|
      object3 Size
        ("source" := string)
        ("width" := number)
        ("height" := number)
     
-- create flickr search URL with provided tag     
createPhotoSearchURL : String -> String
createPhotoSearchURL tag =
  let args = "&method=flickr.photos.search&sort=random&per_page=10&tags="
  in  
    if tag == "" then "" else createFlickrURL args ++ tag
        
-- given a inputted tag, create a flickr search http get request
--createPhotoSearchRequest : String -> Http.Request String
createPhotoSearchRequest : String -> String
createPhotoSearchRequest tag =
--  Http.get (createPhotoSearchURL tag)
  Http.getString (createPhotoSearchURL tag)

-- return basic flickr api URL added to provided tags
createFlickrURL : String -> String
createFlickrURL args = 
  "https://api.flickr.com/services/rest/?format=json"
    ++ "&nojsoncallback=1&api_key=9be5b08cd8168fa82d136aa55f1fdb3c"
    ++ args

toSizeRequestURL : Http.Response String -> String
toSizeRequestURL json =
  case decodeResponse photoList json of
    Err _ ->
      ""
    
    Ok photos ->
      case photos of
        photo :: _ ->
          createFlickrURL "&method=flickr.photos.getSizes&photo_id=" 
            ++ photo.id
            
        [] -> ""
        
convertRespToReq : Http.Response String -> Http.Request String
convertRespToReq json =
  Http.get <| toSizeRequestURL json

convertRespToString : Http.Response String -> Http.Request String
convertRespToString json =
  Http.get <| toSizeRequestURL json

type alias Photo = { id: String, title: String }

photoList : Decoder (List Photo)
photoList =
  at ["photos", "photo"] <| list <|
    object2 Photo
      ("id" := string)
      ("title" := string)
      