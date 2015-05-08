import Signal
import Graphics.Input.Field as Field
import Graphics.Element (flow, down, Element)
import Text
import String 
import Http
import Json.Decode (decodeString, oneOf, int, string, customDecoder, at, list, object2, object3, (:=), Decoder)

tagChnl : Signal.Channel Field.Content
tagChnl = Signal.channel Field.noContent

dbgChnl : Signal.Channel String
dbgChnl = Signal.channel ""

main : Signal Element
main = 
--  Signal.map2 
  Signal.map3 
    view (Signal.subscribe tagChnl) results (Signal.subscribe dbgChnl)

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
        , Text.leftAligned <| Text.fromString imgSrcDetails --"123" 
        , Text.leftAligned <| Text.fromString dbgInfo
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
          resultsSendReq 
          <|
            Signal.map
              toSizeRequest 
                (resultsSendReq <| resultsPhotoReq resultsInput)
--                (resultsSendReq (resultsPhotoReq resultsInput))
        
--NOTE: this works
--  Signal.subscribe tagChnl
--    |> Signal.map .string
--    |> Signal.dropRepeats
--    |> Signal.map toPhotoRequest
--    |> Http.send
--    |> Signal.map toSizeRequest
--    |> Http.send
--    |> Signal.map toPhotoSource

--NOTE: this works
--  resultsInput
--    |> resultsPhotoReq
--    |> resultsSendReq
--    |> Signal.map toSizeRequest
--    |> resultsSendReq
--    |> Signal.map toPhotoSource

----NOTE: this works
--  Signal.map toSizeRequest  
----  NOTE: brackets are important
--    (resultsSendReq (resultsPhotoReq resultsInput))
----    |> Signal.map toSizeRequest
--    |> resultsSendReq
--    |> Signal.map toPhotoSource

----NOTE: this works
--  Signal.map toSizeRequest  
----  NOTE: brackets are important
--    (resultsSendReq (resultsPhotoReq resultsInput))
--    |> resultsSendReq
--    |> Signal.map toPhotoSource

        
resultsInput : Signal String
resultsInput = 
  Signal.dropRepeats <|
  Signal.map .string (Signal.subscribe tagChnl)

resultsPhotoReqTest : Signal String -> Signal String
resultsPhotoReqTest input =
  Signal.map toPhotoRequestURL input
  
resultsPhotoReq : Signal String -> Signal (Http.Request String)
resultsPhotoReq input =
  Signal.map toPhotoRequest input
  
resultsSendReq : Signal (Http.Request String) -> Signal (Http.Response String)
resultsSendReq req =
  Http.send req
  
-- from http response, get "source" of first item (or return Nothing)
toPhotoSource : Http.Response String -> Maybe String
toPhotoSource json = 
  case decodeResponse sizeList json of
    Err msg ->
      Nothing
    
    Ok sizes ->
      case sizes of
        size :: _ -> Just size.source
        [] -> Nothing

decodeResponse : Decoder a -> Http.Response String -> Result String a
decodeResponse decoder response =
  case response of 
    Http.Success str ->
      decodeString decoder str
    
    _ -> Err (toString response)
    
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
     
-- create flickr URL with provided tag     
toPhotoRequestURL : String -> String
toPhotoRequestURL tag =
  let args = "&method=flickr.photos.search&sort=random&per_page=10&tags="
  in  
    if tag == "" then "" else flickrRequest args ++ tag
        
-- given a inputted tag, make a flickr api http get request
toPhotoRequest : String -> Http.Request String
toPhotoRequest tag =
  Http.get (toPhotoRequestURL tag)

-- return basic flickr api URL added to provided tags
flickrRequest : String -> String
flickrRequest args = 
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
          flickrRequest "&method=flickr.photos.getSizes&photo_id=" 
            ++ photo.id
            
        [] -> ""
        
toSizeRequest : Http.Response String -> Http.Request String
toSizeRequest json =
  Http.get <| toSizeRequestURL json

type alias Photo = { id: String, title: String }

photoList : Decoder (List Photo)
photoList =
  at ["photos", "photo"] <| list <|
    object2 Photo
      ("id" := string)
      ("title" := string)
      