import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)
import Time
import Window

-- VIEW

view : Int -> String -> String -> List String -> List String -> Html
view height searchString firstResult results details =
  div [ style (imgStyle height "") ]
    [ input
        [ placeholder "Files Query"
        , Attr.value searchString
        , on "input" targetValue (Signal.message queryChnl.address)
        , style myStyle
        ]
        [],
        div [] [ 
         text firstResult 
        ]
        ,
        div [] [
          text <| resultsAsString2 results
        ],
        div [] [
          text <| resultsAsString2 details
        ]
    ]

resultsAsString2 : List String -> String
resultsAsString2 results = String.join ", " results

resultsAsString : List String -> String
resultsAsString strings = List.foldr 
  --  (++) 
  (appendResults)  
  "" strings
  
appendResults : String -> String -> String
appendResults a b = a ++ ", " ++ b 

myStyle : List (String, String)
myStyle =
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


imgStyle : Int -> String -> List (String, String)
imgStyle h src =
    [ ("background-image", "url('" ++ src ++ "')")
    , ("background-repeat", "no-repeat")
    , ("background-attachment", "fixed")
    , ("background-position", "center")
    , ("width", "100%")
    , ("height", toString h ++ "px")
    ]


-- WIRING

main : Signal Html
main =
  Signal.map5 view Window.height queryChnl.signal resultChnl.signal resultsChnl.signal detailsChnl.signal

queryChnl : Signal.Mailbox String
queryChnl = Signal.mailbox "red-info.txt"

resultChnl : Signal.Mailbox String
resultChnl = Signal.mailbox "filename"

resultsChnl : Signal.Mailbox (List String)
resultsChnl = Signal.mailbox ["filename"]

detailsChnl : Signal.Mailbox (List String)
detailsChnl = Signal.mailbox ["details"]

port getVidInfoFileDetails : Signal (Task Http.Error ())        
port getVidInfoFileDetails =
  Signal.map getFileDetails queryChnl.signal
    |> Signal.sampleOn trigger
    |> Signal.map (\task -> task `andThen` 
                    Signal.send detailsChnl.address)

--port 
getVidInfoFirstFile : Signal (Task Http.Error ())
--port 
getVidInfoFirstFile =
      Signal.map getFirstFileName queryChnl.signal
        |> Signal.sampleOn trigger
        |> Signal.map (\task -> task `andThen` Signal.send 
            resultChnl.address)

--port 
getVidInfoFiles : Signal (Task Http.Error ())
--port 
getVidInfoFiles =
      Signal.map getFileNames queryChnl.signal
      |> Signal.sampleOn trigger    
      |> Signal.map (\task -> task `andThen` Signal.send 
          resultsChnl.address) 

--port 
getVidInfoFilesAsString : Signal (Task Http.Error ())
--port 
getVidInfoFilesAsString =
  Signal.map getFileNamesAsString queryChnl.signal
    |> Signal.sampleOn trigger
    |> Signal.map (\task -> task `andThen` Signal.send    
        resultChnl.address)
        

trigger : Signal Bool
trigger =
  let stamped = Time.timestamp queryChnl.signal
      delayed = Time.delay 500 stamped
  in
--      Signal.map2 (==) stamped delayed
--        |> Signal.filter identity True
      
  Signal.filter identity True
    (Signal.map2 (==) stamped delayed)


getFileDetails : String -> Task Http.Error (List String)
getFileDetails string = 
  Http.get 
--    detailsList
--    filenameDecoder
--    titleDetailsDecoder
    titleDetailsList
    vidInfoURL `andThen` 
--      getDetail
--      getDetails
      getTitleDetails
    
--titleDetailsDecoder : Json.Decoder (List String)    
--getDetails : List String -> Task Http.Error (List String)


getFileNames : String -> Task Http.Error (List String)
getFileNames string = 
  Http.get stringList
    vidInfoFilesURL `andThen` getStrings
    
getFirstFileName : String -> Task Http.Error String
getFirstFileName string = 
  Http.get stringList
    vidInfoFilesURL `andThen` getFirstString

getFileNamesAsString : String -> Task Http.Error String
getFileNamesAsString string = 
  Http.getString vidInfoFilesURL        

--getFileNamesAsString string = getStringCors "http://localhost:9090/vidInfo/files"

-- JSON DECODERS

type alias TitleDetail =
    { 
--      titleNumber : String
--    , length: String
      titleNumber : Int
    , length: Float
    }

type alias Photo =
    { id : String
    , title : String
    }

type alias Size =
    { source : String
    , width : Int
    , height : Int
    }
    
--detailsList : Json.Decoder (List String)    
--detailsList = Json.list Json.string
filenameDecoder : Json.Decoder (String)    
filenameDecoder = "fileName" := Json.string

titleDetailsDecoder : Json.Decoder (List String)    
titleDetailsDecoder = 
--  Debug.log "titleDetails" <|
    "titleDetails" := Json.list Json.string
    
titleDetailsList : Json.Decoder (List TitleDetail)
titleDetailsList =
  Json.at ["wrapper", "titleDetails"] <| 
    Json.list <|
      Json.object2 TitleDetail
--        ("titleNumber" := Json.string)
--        ("length" := Json.string)
        ("titleNumber" := Json.int)
        ("length" := Json.float)
  

stringList : Json.Decoder (List String)    
stringList = Json.list Json.string

photoList : Json.Decoder (List Photo)
photoList =
  Json.at ["photos","photo"] <| Json.list <|
      Json.object2 Photo
        ("id" := Json.string)
        ("title" := Json.string)


sizeList : Json.Decoder (List Size)
sizeList =
  let number =
        Json.oneOf [ Json.int, Json.customDecoder Json.string String.toInt ]
  in
      Json.at ["sizes","size"] <| Json.list <|
          Json.object3 Size
            ("source" := Json.string)
            ("width" := number)
            ("height" := number)


--  FLICKR URLS

createFlickrURL : String -> List (String, String) -> String
createFlickrURL method args =
  Http.url "https://api.flickr.com/services/rest/" <|
    [ ("format", "json")
    , ("nojsoncallback", "1")
    , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
    , ("method", "flickr.photos." ++ method)
    ] ++ args

vidInfoURL : String
vidInfoURL =
--  Http.url "http://localhost:9090/vidInfo" []
  Http.url "http://localhost:9090/vidInfoWrapped" []

vidInfoFilesURL : String
vidInfoFilesURL =
  Http.url "http://localhost:9090/vidInfo/files" []

-- HANDLE RESPONSES

getDetail : String -> Task Http.Error (List String)
getDetail string =
--    succeed ["file details"]
    succeed [string]

getDetails : List String -> Task Http.Error (List String)
getDetails strings =
  case strings of
    string :: _ -> succeed [
      Debug.log "files dets" "file details"
      ]
    [] ->
--      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")
        succeed ["no details found"]
        
getTitleDetails : List TitleDetail -> Task Http.Error (List String)
getTitleDetails details =
  case details of
    string :: _ -> succeed 
      (List.map (toString) details)
--      [
----      Debug.log "files dets" "file details"
--        "dummy title dets"
--      ]
    [] ->
--      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")
        succeed ["no details found"]
        

getStrings : List String -> Task Http.Error (List String)
getStrings strings =
  case strings of
    string :: _ -> succeed strings
    [] ->
      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")

getFirstString : List String -> Task Http.Error String
getFirstString strings =
  case strings of
    string :: _ -> succeed string
    [] ->
      fail (Http.UnexpectedPayload "expecting 1 or more strings from server")

pickSize : (Int,Int) -> List Size -> Task Http.Error String
pickSize (width,height) sizes =
  let sizeRating size =
        let penalty =
              if size.width > width || size.height > height then 400 else 0
        in
            abs (width - size.width) + abs (height - size.height) + penalty
  in
      case List.sortBy sizeRating sizes of
        size :: _ -> succeed size.source
        [] ->
          fail (Http.UnexpectedPayload "expecting 1 or more image sizes to choose from")


--getFlickrImage : (Int,Int) -> String -> Task Http.Error String
--getFlickrImage dimensions tag =
--  let searchArgs =
--        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
--  in
--      Http.get photoList (createFlickrURL "search" searchArgs)
--        `andThen`
--            selectPhoto
--        `andThen` \photo ->
--            Http.get sizeList (createFlickrURL "getSizes" [ ("photo_id", photo.id) ])
--        `andThen`
--            pickSize dimensions
--


-- GETSTRING FOR CUSTOM HEADERS

getStringCors : String -> Task Http.Error String
getStringCors url =
  let request =
        { verb = "GET"
        , headers = [
--            ("Accept", "*/*")
        ]
        , url = url
        , body = Http.empty
        }
  in
      mapError promoteError (Http.send Http.defaultSettings request)
        `andThen` handleResponse succeed

promoteError : Http.RawError -> Http.Error
promoteError rawError =
  case rawError of
    Http.RawTimeout -> Http.Timeout
    Http.RawNetworkError -> Http.NetworkError
    
handleResponse : (String -> Task Http.Error a) -> Http.Response -> Task Http.Error a
handleResponse handle response =
  if 200 <= response.status && response.status < 300 then

      case response.value of
        Http.Text str ->
            handle str

        _ ->
            fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")

  else
      fail (Http.BadResponse response.status response.statusText)    

