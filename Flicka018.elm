module Flicka018 exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (field)
import String
import Task exposing (..)
import Time
import Window

-- VIEW

-- view : Int -> String -> String -> Html
-- view height string imgUrl =
--   div [ style (imgStyle height imgUrl) ]
--     [ input
--         [ placeholder "Flickr Query"
--         , Attr.value string
--         -- , on "input" targetValue (Signal.message queryChnl.address)
--         , style myStyle
--         ]
--         []
--     ]


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

-- new for 018

type Msg = 
      Info (Result Http.Error (List Photo))
    | Photos (List Photo)
    | Pic Photo
    | Sizes (List Size)
    | Source String
    | InfoS (Result Http.Error String)
    | Search String

type alias Model = {
    count     : Int
  , info      : String
  , photos : List Photo
  , photo  : Photo
  , sizes  : List Size
  , source : String
  }

main = Html.program { 
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions 
  }

model = { 
    count = 0
  , info = "initial state"
  , photos = []
  , photo =  { id = "1", title = "init" }
  , sizes = []
  , source = ""
  }

init = (
    model
  , Cmd.none)

subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg )
update msg model = 
  let 
    flikr s = 
      -- Cmd.none
      -- Task.perform Photos (Task.succeed [])
      -- Task.perform Photos (getFlickrImageChain (10, 10) s)
      -- Task.perform Pic (getFlickrImageSingle (10, 10) s)
      -- Task.perform Sizes (getFlickrImageSizes (10, 10) s)
      Task.perform Source (getFlickrImage (10, 10) s)
  in
  case msg of 
    Info r -> 
      case r of 
        Ok ps ->  ( {model | photos = ps}, Cmd.none)
        Err s ->  (model, Cmd.none)
    InfoS a -> 
      ( {model | count = model.count + 1 }, flikr "kitten" )
    Search s ->
      ( {model | count = model.count + 1 }, flikr s)
    Photos ps ->
      ( {model | count = model.count + 1, photos = ps }, Cmd.none)
    Pic pic ->
      ( { model | count = model.count + 1, photo = pic }, Cmd.none)
    Sizes zs ->
      ( { model | count = model.count + 1, sizes = zs }, Cmd.none )
    Source s ->
      ( { model | count = model.count + 1, source = s }, Cmd.none )
    
view : Model -> Html Msg
view model =
  div [ 
        -- style (imgStyle height "") 
        class "contain"
      ]
    [ 
      input
      [ placeholder "Flickr Query"
      -- -- , Attr.value string
      , onInput Search -- (Signal.message queryChnl.address)
      -- , style myStyle
      ]
      []
    , ol []
      (List.map (\p -> option [] [text <| toString p] ) model.photos)
    , text <| "pic: " ++ ( toString model.photo )
    , ol []
      (List.map (\sz -> option [] [text <| toString sz] ) model.sizes)
    , text model.source
    
    ]
-- view : Int -> String -> String -> Html
-- view height string imgUrl =
--   div [ style (imgStyle height imgUrl) ]
--     [ input
--         [ placeholder "Flickr Query"
--         , Attr.value string
--         -- , on "input" targetValue (Signal.message queryChnl.address)
--         , style myStyle
--         ]
--         []
--     ]

-- main : Signal Html
-- main =
--   Signal.map3 view Window.height queryChnl.signal resultsChnl.signal

-- queryChnl : Signal.Mailbox String
-- queryChnl = Signal.mailbox ""

-- resultsChnl : Signal.Mailbox String
-- resultsChnl = Signal.mailbox "waiting.gif"

-- getVidInfoFilesAsString =
--   Signal.map getFileNamesAsString queryChnl.signal
--     |> Signal.sampleOn trigger
--     |> Signal.map (\task -> task `andThen` Signal.send    
--         resultChnl.address)

-- getFileNamesAsStringCmd : Cmd Msg
-- getFileNamesAsStringCmd = Http.send Info <| getFileNamesAsStringReq ""

-- port updateResults : Signal (Task Http.Error ())
-- port updateResults =
--   Signal.map2 getFlickrImage Window.dimensions queryChnl.signal
--     |> Signal.sampleOn trigger
--     |> Signal.map (\task -> task `andThen` Signal.send resultsChnl.address)


-- trigger : Signal Bool
-- trigger =
--   let stamped = Time.timestamp queryChnl.signal
--       delayed = Time.delay 500 stamped
--   in
--       Signal.map2 (==) stamped delayed
--         |> Signal.filter identity True


-- getFlickrImage : (Int,Int) -> String -> Task Http.Error String
-- getFlickrImage dimensions tag =
--   let searchArgs =
--         [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
--   in
--       Http.get photoList (createFlickrURL "search" searchArgs)
--         `andThen`
--             selectPhoto
--         `andThen` \photo ->
--             Http.get sizeList (createFlickrURL "getSizes" [ ("photo_id", photo.id) ])
--         `andThen`
--             pickSize dimensions

-- getFlickrImage : (Int,Int) -> String -> Task Http.Error String
getFlickrImageBasic : (Int,Int) -> String -> Cmd Msg
getFlickrImageBasic dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in 
    Http.send Info <|
      Http.get 
        (createFlickrURL "search" searchArgs)
        photoList 
--         `andThen`
--             selectPhoto
--         `andThen` \photo ->
--             Http.get sizeList (createFlickrURL "getSizes" [ ("photo_id", photo.id) ])
--         `andThen`
--             pickSize dimensions

-- getFlickrImage : (Int,Int) -> String -> Task x Msg
getFlickrImageChain : (Int,Int) -> String -> Task x (List Photo)
getFlickrImageChain dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
    --  toTask : Request a -> Task Error a
    -- Task Http.Error (List Photo)
    (Http.toTask (Http.get (createFlickrURL "search" searchArgs) photoList ))
      |>
        -- Task x a -> Task x (List b) 
        -- Task Error a -> Task Error (List b) 
        (Task.andThen           
          (\ps -> Task.succeed ps
            -- Task.fail Http.Timeout -- forces a failure
          -- [ { id = "1", title = "success"} ] -- default data
          )) 

      -- pass empty list if an error occurred
      -- Task a (List b) -> Task x (List b)
      -- Task Error (List b) -> Task Never (List b)
      |> (onError (\_ -> succeed [
            { id = "2"
            , title = "error"
            }
          ]))

getFlickrImageSingle : (Int,Int) -> String -> Task x Photo
getFlickrImageSingle dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
    --  toTask : Request a -> Task Error a
    -- Task Http.Error (List Photo)
    (Http.toTask (Http.get (createFlickrURL "search" searchArgs) photoList ))
      |>
        -- Task Error (List a) -> Task Error a
        (Task.andThen selectPhoto)
      -- pass default data if an error occurred
      -- Task Error ?? -> Task Never a
      |> (onError (\_ -> succeed 
            -- [
            { id = "2"
            , title = "error"
            }
            -- ]
          ))

getFlickrImageSizes : (Int,Int) -> String -> Task x (List Size)
getFlickrImageSizes dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
    --  toTask : Request a -> Task Error a
    -- Task Http.Error (List Photo)
    (Http.toTask (Http.get (createFlickrURL "search" searchArgs) photoList ))
      |> (Task.andThen selectPhoto)
      |> (Task.andThen 
            (\photo -> 
              (Http.toTask 
                (Http.get (createFlickrURL "getSizes" [ ("photo_id", photo.id) ]) sizeList )))
         )
      |> (onError (\_ -> succeed [] ))

getFlickrImage : (Int,Int) -> String -> Task x String
getFlickrImage dimensions tag =
  let searchArgs =
        [ ("sort", "random"), ("per_page", "10"), ("tags", tag) ]
  in
    --  toTask : Request a -> Task Error a
    -- Task Http.Error (List Photo)
    (Http.toTask (Http.get (createFlickrURL "search" searchArgs) photoList ))
      |> (Task.andThen selectPhoto)
      |> (Task.andThen 
            (\photo -> 
              (Http.toTask 
                (Http.get (createFlickrURL "getSizes" [ ("photo_id", photo.id) ]) sizeList )))
         )
      |> (Task.andThen <| pickSize dimensions)
      |> (onError (\_ -> succeed "no photo size" ))
             
-- from upgrade doc
        -- onError : (x -> Task y a) -> Task x a -> Task y a
        -- |> Task.onError (\error -> Task.succeed DidNotLoad)

-- Function `perform` is expecting the 2nd argument to be:
--     Task Never (List Photo)
-- But it is:
--     Task Http.Error (List Photo)

sendBlankPhotos :  a -> Task.Task x (List Photo)
sendBlankPhotos = (\_ -> Task.succeed [])

blankPhotos : Cmd Msg
  -- perform : (a -> msg) -> Task Never a -> Cmd msg
blankPhotos = Task.perform Photos (Task.succeed [])

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

photoList : Json.Decode.Decoder (List Photo)
photoList =
  Json.Decode.at ["photos","photo"] <| Json.Decode.list <|
      Json.Decode.map2 Photo
        (field "id" Json.Decode.string)
        (field "title" Json.Decode.string)

sizeList : Json.Decode.Decoder (List Size)
sizeList =
  let number =
        Json.Decode.oneOf [ Json.Decode.int, customDecoder Json.Decode.string String.toInt ]
  in
      Json.Decode.at ["sizes","size"] <| Json.Decode.list <|
          Json.Decode.map3 Size
            (field "source" Json.Decode.string)
            (field "width" number)
            (field "height" number)

customDecoder decoder toResult = 
   Json.Decode.andThen (\a ->
                          case toResult a of 
                            Ok b -> Json.Decode.succeed b
                            Err err -> Json.Decode.fail err
                       )
                       decoder


-- HANDLE RESPONSES
selectPhoto : List Photo -> Task Http.Error Photo
selectPhoto photos =
  case photos of
    photo :: _ -> succeed photo
    [] -> fail Http.Timeout -- easier to return this error in 018 for now
--       fail (Http.UnexpectedPayload "expecting 1 or more photos from Flickr")

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
        [] -> fail Http.Timeout -- easier to return this error in 018 for now
          -- fail (Http.UnexpectedPayload "expecting 1 or more image sizes to choose from")


--  FLICKR URLS

-- createFlickrURL : String -> List (String, String) -> String
-- createFlickrURL method args =
--   Http.url "https://api.flickr.com/services/rest/" <|
--     [ ("format", "json")
--     , ("nojsoncallback", "1")
--     , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
--     , ("method", "flickr.photos." ++ method)
--     ] ++ args

-- how this used to work ...
-- http://package.elm-lang.org/packages/evancz/elm-http/3.0.1/Http
-- createFlickrURL : String -> List (String, String) -> Http.Request (List Photo)
-- createFlickrURL method args =
-- -- put : String -> Body -> Request ()
-- -- put url body =
--   Http.request
--     { method = "GET"
--     , headers = []
--     , url = "https://api.flickr.com/services/rest/"
--     , body = Http.emptyBody
--     -- , expect = Http.expectStringResponse (\_ -> Ok ())
--     , expect = Http.expectJson photoList
--     , timeout = Nothing
--     , withCredentials = False
--     }

createFlickrURL : String -> List (String, String) -> String
createFlickrURL method args =
  url "https://api.flickr.com/services/rest/" <|
    [ ("format", "json")
    , ("nojsoncallback", "1")
    , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
    , ("method", "flickr.photos." ++ method)
    ] ++ args

url : String -> List (String,String) -> String
url baseUrl args =
  case args of
    [] -> baseUrl
    _  -> baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)

queryPair : (String,String) -> String
queryPair (key,value) =
  queryEscape key ++ "=" ++ queryEscape value

queryEscape : String -> String
queryEscape string =
  String.join "+" (String.split "%20" (Http.encodeUri string))
