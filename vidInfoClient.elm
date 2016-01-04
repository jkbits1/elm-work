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

view : Int -> String -> String -> Html
view height searchString imgUrl =
  div [ style (imgStyle height imgUrl) ]
    [ input
        [ placeholder "Files Query"
        , Attr.value searchString
        , on "input" targetValue (Signal.message queryChnl.address)
        , style myStyle
        ]
        []
    ]


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
  Signal.map3 view Window.height queryChnl.signal resultsChnl.signal

queryChnl : Signal.Mailbox String
queryChnl = Signal.mailbox ""

resultsChnl : Signal.Mailbox String
resultsChnl = Signal.mailbox "waiting.gif"


--port updateResults : Signal (Task Http.Error ())
--port updateResults =
--  Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
--    |> Signal.sampleOn trigger
--    |> Signal.map (\task -> task `andThen` Signal.send resultsChnl.address)

port updateResultsVidInfo : Signal (Task Http.Error ())
port updateResultsVidInfo =
  Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
    |> Signal.sampleOn trigger
    |> Signal.map (\task -> task `andThen` Signal.send resultsChnl.address)


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

getFileNamesAsString : Task Http.Error String
getFileNamesAsString = Http.getString "localhost:9090/vidInfo/files"

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

createInfoURL : String -> List (String, String) -> String
createInfoURL method args =
  Http.url "http://localhost:9090/vidInfo" []


-- HANDLE RESPONSES

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
