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

view : Int -> String -> String -> List String -> Html
view height searchString firstResult results =
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
          text <| resultsAsString results
        ]
    ]

resultsAsString : List String -> String
resultsAsString strings = List.foldr (++) "" strings

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
  Signal.map4 view Window.height queryChnl.signal resultChnl.signal resultsChnl.signal

queryChnl : Signal.Mailbox String
queryChnl = Signal.mailbox ""

resultChnl : Signal.Mailbox String
resultChnl = Signal.mailbox "waiting.gif"

resultsChnl : Signal.Mailbox (List String)
resultsChnl = Signal.mailbox ["waiting.gif"]


--port updateResults : Signal (Task Http.Error ())
--port updateResults =
--  Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
--    |> Signal.sampleOn trigger
--    |> Signal.map (\task -> task `andThen` Signal.send resultsChnl.address)

--port getVidInfoFilesAsString : Signal (Task Http.Error ())
--port getVidInfoFilesAsString =
----  Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
--  Signal.map getFileNamesAsString queryChnl.signal
--    |> Signal.sampleOn trigger
--    |> Signal.map (\task -> task `andThen` Signal.send resultsChnl.address)

--port getVidInfoFirstFile : Signal (Task Http.Error ())
--port getVidInfoFirstFile =
----  Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
----  Signal.map getFileNames queryChnl.signal
--  Signal.map getFirstFileName queryChnl.signal
--    |> Signal.sampleOn trigger
--    |> Signal.map (\task -> task `andThen` Signal.send resultsChnl.address)

port getVidInfoFirstFile : Signal (Task Http.Error ())
port getVidInfoFirstFile =
--  Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
--  Signal.map getFileNames queryChnl.signal
  Signal.map (\task -> task `andThen` Signal.send resultChnl.address)
    (
      Signal.map getFirstFileName queryChnl.signal
        |> Signal.sampleOn trigger
    )

port getVidInfoFiles : Signal (Task Http.Error ())
port getVidInfoFiles =
--  Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
    Signal.map (\task -> task `andThen` Signal.send resultsChnl.address) 
      (
        Signal.map getFileNames queryChnl.signal
        |> Signal.sampleOn trigger    
      )

--getFileNames : String -> Task Http.Error (List String)


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

trigger : Signal Bool
trigger =
  let stamped = Time.timestamp queryChnl.signal
      delayed = Time.delay 500 stamped
  in
      Signal.map2 (==) stamped delayed
        |> Signal.filter identity True


getFlickrImage : (Int,Int) -> String -> Task Http.Error String
getFlickrImage dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
      Http.get photoList (createFlickrURL "search" searchArgs)
        `andThen`
            selectPhoto
        `andThen` \photo ->
            Http.get sizeList (createFlickrURL "getSizes" [ ("photo_id", photo.id) ])
        `andThen`
            pickSize dimensions

getFileNames : String -> Task Http.Error (List String)
getFileNames string = 
  Http.get stringList
    -- "http://localhost:9090/vidInfo/files"
    --    vidInfoURL        
    vidInfoFilesURL `andThen` getStrings
    
getFirstFileName : String -> Task Http.Error String
getFirstFileName string = 
  Http.get stringList
    -- "http://localhost:9090/vidInfo/files"
    --    vidInfoURL        
    vidInfoFilesURL `andThen` getFirstString

getFileNamesAsString : String -> Task Http.Error String
getFileNamesAsString string = 
  Http.getString -- "http://localhost:9090/vidInfo/files"
--    vidInfoURL        
    vidInfoFilesURL        

--getFileNamesAsString string = getStringCors "http://localhost:9090/vidInfo/files"

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

-- JSON DECODERS

type alias Photo =
    { id : String
    , title : String
    }


type alias Size =
    { source : String
    , width : Int
    , height : Int
    }
    
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
  Http.url "http://localhost:9090/vidInfo" []

vidInfoFilesURL : String
vidInfoFilesURL =
  Http.url "http://localhost:9090/vidInfo/files" []


-- HANDLE RESPONSES

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

selectPhoto : List Photo -> Task Http.Error Photo
selectPhoto photos =
  case photos of
    photo :: _ -> succeed photo
    [] ->
      fail (Http.UnexpectedPayload "expecting 1 or more photos from Flickr")


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
