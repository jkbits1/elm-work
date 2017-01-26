import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (field)
import String
import Task exposing (..)
import Time
import JsonBits exposing (..)
import List exposing (..)
-- import Window


-- queryChnl : Signal.Mailbox String
-- queryChnl = Signal.mailbox "red-info.txt"

-- resultChnl : Signal.Mailbox String
-- resultChnl = Signal.mailbox "filename"

-- resultsChnl : Signal.Mailbox (List String)
-- resultsChnl = Signal.mailbox ["filename"]

-- detailsChnl : Signal.Mailbox (List String)
-- detailsChnl = Signal.mailbox ["details"]

-- specificDetailsChnl : Signal.Mailbox (List TitleDetail)
-- specificDetailsChnl = Signal.mailbox [TitleDetail 0 0.0]

-- type Msg =
--   -- queryChnl : Signal.Mailbox String
--   queryChnl = Signal.mailbox "red-info.txt"

-- resultChnl : Signal.Mailbox String
-- resultChnl = Signal.mailbox "filename"

-- resultsChnl : Signal.Mailbox (List String)
-- resultsChnl = Signal.mailbox ["filename"]

-- detailsChnl : Signal.Mailbox (List String)
-- detailsChnl = Signal.mailbox ["details"]

-- specificDetailsChnl : Signal.Mailbox (List TitleDetail)
-- specificDetailsChnl = Signal.mailbox [TitleDetail 0 0.0]

infoListItems : List a -> List (Html Msg)
infoListItems xs = 
  List.map (\s -> li [] [text <| toString s]) xs
  -- [ text <| toString model.info
  -- , ul [] [
  --     li [] [ text "2nd level item" ]
  --   ] 
  -- ]

              -- li []f
              -- <| infoListItems model


-- VIEW

-- view : Int -> String -> String -> List String -> List String -> 
--         List TitleDetail -> Html
-- view height searchString firstResult results details specifics =
view model =
  div [ 
        -- style (imgStyle height "") 
      ]
    [ 
      div []
        [ 
            -- placeholder "test get"
        -- , Attr.value searchString
        -- , on "input" targetValue (Signal.message queryChnl)
        -- , onInput queryChnl
        -- , style myStyle
          button [ onClick ButtonGet ]              [ text "send request" ]
        , button [ onClick ButtonGetFirstFileName ] [ text "send request - first file name" ]
        , button [ onClick ButtonGetFileNames ]     [ text "send request - file names" ]
        , button [ onClick ButtonGetFileNames ]     [ text "send request - file names" ]
        , button [ onClick ButtonGetFileDetails ]   [ text "send request - file details" ]
        , button [ onClick ButtonGetFileDetails3 ]   [ text "send request - file details3" ]
        , button [ onClick ButtonGetFileDetailsWrapped ]   [ text "send request - file details wrapped" ]        
        ]
      , div []
        [
          text <| toString model.count
        ]

      , div []
        [
          text <| toString model.info
        ]

      , div []
        [
          ul []
          -- [ 
            -- li []
              -- <| 
                <| infoListItems model.fileNames
          -- ]
        ]
      , div []
        [
          ul [] <| infoListItems model.xvals
        ]
      , div []
        [
          ul [] <| infoListItems model.titleDetails
        ]
        
      -- ,
      -- input
      --   [ placeholder "Files Query"
      --   , Attr.value searchString
      --   -- , on "input" targetValue (Signal.message queryChnl)
      --   -- , onInput queryChnl
      --   , style myStyle
      --   ]
      --   [],
      --   div [] [ 
      --    text firstResult 
      --   ]
      --   ,
      --   div [] [
      --     text <| resultsAsString2 results
      --   ],
      --   div [] [
      --     text <| resultsAsString2 details
      --   ],
      --   div [] [
      --     text <| 
      --       toString <| (firstSpecific specifics).titleNumber,
      --     text ",",
      --     text <| 
      --       Debug.log "length"
      --         (
      --           toString <| (firstSpecific specifics).length
      --         )
      --   ]
    ]
    
firstSpecific : List TitleDetail -> TitleDetail
firstSpecific details = 
  let m = List.head details
  in 
    case m of 
      Just detail -> detail
      Nothing     -> TitleDetail 0 0.0

--specificsAsString : List TitleDetail -> String
--specificsAsString results = String.join ", " results

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



-- trigger : Signal Bool
-- trigger =
--   let stamped = Time.timestamp queryChnl.signal
--       delayed = Time.delay 500 stamped
--   in
-- --      Signal.map2 (==) stamped delayed
-- --        |> Signal.filter identity True
      
--   Signal.filter identity True
--     (Signal.map2 (==) stamped delayed)


-- port getVidInfoFileDetails : Signal (Task Http.Error ())        
-- port getVidInfoFileDetails =
--   Signal.map getFileDetails queryChnl.signal
--     |> Signal.sampleOn trigger
--     |> Signal.map (\task -> task `andThen` 
--                     Signal.send detailsChnl)

--port
-- getVidInfoFileSpecifics : Signal (Task Http.Error ())
-- getVidInfoFileSpecifics =
--   Signal.map getFileSpecifics queryChnl.signal
--     |> Signal.sampleOn trigger
--     |> Signal.map (\task -> task `andThen` 
--                     Signal.send specificDetailsChnl)

-- DONE
--port 
-- getVidInfoFirstFile : Signal (Task Http.Error ())
-- getVidInfoFirstFile =
--       Signal.map getFirstFileName queryChnl.signal
--         |> Signal.sampleOn trigger
--         |> Signal.map (\task -> task `andThen` Signal.send 
--             resultChnl)

-- DONE
--port 
-- getVidInfoFiles : Signal (Task Http.Error ())
-- getVidInfoFiles =
--       Signal.map getFileNames queryChnl.signal
--       |> Signal.sampleOn trigger    
--       |> Signal.map (\task -> task `andThen` Signal.send 
--           resultsChnl) 

-- DONE
--port 
-- getVidInfoFilesAsString : Signal (Task Http.Error ())
-- getVidInfoFilesAsString =
--   Signal.map getFileNamesAsString queryChnl.signal
--     |> Signal.sampleOn trigger
--     |> Signal.map (\task -> task `andThen` Signal.send    
--         resultChnl)

-- getFileSpecifics : String -> Task Http.Error (List TitleDetail)
-- getFileSpecifics string = 
--   Http.get
--     titleDetailsList
-- --      (vidInfoURL ++ "\\" ++ string)
--       (vidInfoURL)
--         `andThen` getTitleSpecifics

    
--titleDetailsDecoder : Json.Decoder (List String)    
--getDetails : List String -> Task Http.Error (List String)


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
    
-- filenameDecoder : Json.DecodeDecoder (String)    
-- filenameDecoder = "fileName" |> field Json.Decodestring

-- titleDetailsDecoder : Json.DecodeDecoder (List String)    
-- titleDetailsDecoder = 
-- --  Debug.log "titleDetails" <|
--     "titleDetails" |> field Json.Decodelist Json.Decodestring
    
-- photoList : Json.DecodeDecoder (List Photo)
-- photoList =
--   Json.Decodeat ["photos","photo"] <| Json.Decodelist <|
--       Json.Decodemap2 Photo
--         ("id" |> field Json.Decodestring)
--         ("title" |> field Json.Decodestring)


-- sizeList : Json.DecodeDecoder (List Size)
-- sizeList =
--   let number =
--         -- Json.DecodeoneOf [ Json.Decodeint, Json.DecodecustomDecoder Json.Decodestring String.toInt ]
--         Json.DecodeoneOf [ Json.Decodeint, customDecoder Json.Decodestring String.toInt ]
--   in
--       Json.Decodeat ["sizes","size"] <| Json.Decodelist <|
--           Json.Decodemap3 Size
--             ("source" |> field Json.Decodestring)
--             ("width" |> field number)
--             ("height" |> field number)


--  FLICKR URLS

-- createFlickrURL : String -> List (String, String) -> String
-- createFlickrURL method args =
--   Http.url "https://api.flickr.com/services/rest/" <|
--     [ ("format", "json")
--     , ("nojsoncallback", "1")
--     , ("api_key", "9be5b08cd8168fa82d136aa55f1fdb3c")
--     , ("method", "flickr.photos." ++ method)
--     ] ++ args



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

-- pickSize : (Int,Int) -> List Size -> Task Http.Error String
-- pickSize (width,height) sizes =
--   let sizeRating size =
--         let penalty =
--               if size.width > width || size.height > height then 400 else 0
--         in
--             abs (width - size.width) + abs (height - size.height) + penalty
--   in
--       case List.sortBy sizeRating sizes of
--         size :: _ -> succeed size.source
--         [] ->
--           fail (Http.UnexpectedPayload "expecting 1 or more image sizes to choose from")


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

-- getStringCors : String -> Task Http.Error String
-- getStringCors url =
--   let request =
--         { verb = "GET"
--         , headers = [
-- --            ("Accept", "*/*")
--         ]
--         , url = url
--         , body = Http.empty
--         }
--   in
--       mapError promoteError (Http.send Http.defaultSettings request)
--         `andThen` handleResponse succeed

-- promoteError : Http.RawError -> Http.Error
-- promoteError rawError =
--   case rawError of
--     Http.RawTimeout -> Http.Timeout
--     Http.RawNetworkError -> Http.NetworkError
    
-- handleResponse : (String -> Task Http.Error a) -> Http.Response -> Task Http.Error a
-- handleResponse handle response =
--   if 200 <= response.status && response.status < 300 then

--       case response.value of
--         Http.Text str ->
--             handle str

--         _ ->
--             fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")

--   else
--       fail (Http.BadResponse response.status response.statusText)    


-- WIRING

-- main : Signal Html
-- main =
-- --  Signal.map2 view Window.height queryChnl.signal 
--   view <~ Window.height ~ queryChnl.signal ~ resultChnl.signal ~ 
--     resultsChnl.signal ~ detailsChnl.signal ~       
--     specificDetailsChnl.signal

main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }
-- main = Html.beginnerProgram { model = model, view = view, update = update }


-- These functions were removed from the Signal package. 
-- They allow the equivalent of map6 and beyond.
-- (<~) : (a -> b) -> Signal a -> Signal b
-- (<~) = Signal.map  
  
-- (~) : Signal (a -> b) -> Signal a -> Signal b
-- (~) funcs args =
--   Signal.map2 (\f v -> f v) funcs args

--infixl 4 ~

model = { count = 0, info = "test", fileNames = [], xvals = [], titleDetails = [] }

init = (
    model
  , Cmd.none)

subscriptions model = Sub.none


-- code below grabbed from original, now deprecated, package
-- https://github.com/evancz/elm-http/blob/3.0.1/src/Http.elm
-- REQUESTS

{-| Create a properly encoded URL with a [query string][qs]. The first argument is
the portion of the URL before the query string, which is assumed to be
properly encoded already. The second argument is a list of all the
key/value pairs needed for the query string. Both the keys and values
will be appropriately encoded, so they can contain spaces, ampersands, etc.
[qs]: http://en.wikipedia.org/wiki/Query_string
    url "http://example.com/users" [ ("name", "john doe"), ("age", "30") ]
    -- http://example.com/users?name=john+doe&age=30
-}
-- url : String -> List (String,String) -> String
-- url baseUrl args =
--   case args of
--     [] ->
--         baseUrl

--     _ ->
--         baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


-- queryPair : (String,String) -> String
-- queryPair (key,value) =
--   queryEscape key ++ "=" ++ queryEscape value


-- queryEscape : String -> String
-- queryEscape string =
--   String.join "+" (String.split "%20" (uriEncode string))

-- uriEncode : String -> String
-- uriEncode =
--   Native.Http.uriEncode

customDecoder decoder toResult = 
   Json.Decode.andThen
             (\a ->
                   case toResult a of 
                      Ok b -> Json.Decode.succeed b
                      Err err -> Json.Decode.fail err
             )
             decoder


update : Msg -> Model -> (Model, Cmd Msg )
update msg model = 
  case msg of 
    NoOp -> (model, Cmd.none)
    ButtonGet -> 
      ( {model | count = model.count + 1 }, getFileNamesAsStringCmd )
    -- ButtonGet -> ( {model | count = model.count + 1 }, getFirstFileNameNew "string" )
    ButtonGetFirstFileName -> 
      ( {model | count = model.count + 1 }, getFirstFileName "string" )
    ButtonGetFileNames -> 
      ( {model | count = model.count + 1 }, getFileNames "string" )

    ButtonGetFileDetails  -> 
      -- ( {model | count = model.count + 1 }, getFileDetails "red-info.txt" )
      ( {model | count = model.count + 1 }, getFileDetails2 "red-info.txt" )

    ButtonGetFileDetails3 ->
      ( {model | count = model.count + 1 }, getFileDetails3 "red-info.txt" )

    ButtonGetFileDetailsWrapped  ->
      ( {model | count = model.count + 1 }, getFileDetailsWrapped "red-info.txt" )

    Info (Ok jsonInfo) -> ( {model | info = jsonInfo }, Cmd.none)    
    Info (Err _) -> (model, Cmd.none)

    InfoFirstFileName (Ok jsonInfo) -> ( {model | info = jsonInfo }, Cmd.none)    
    InfoFirstFileName (Err _) -> (model, Cmd.none)

    InfoList (Ok jsonInfo) -> 
      let 
        -- item = Maybe.withDefault "" <| head jsonInfo
        items = -- toString 
          jsonInfo
      in 
        ( {model | fileNames = items }, Cmd.none)    
    InfoList (Err _) -> (model, Cmd.none)

  -- | InfoFileDetails (Result Http.Error (List TitleDetail))
    InfoFileDetails1 (Ok jsonInfo) -> ( {model | info = toString jsonInfo }, Cmd.none)    
    -- InfoFileDetails (Ok jsonInfo) -> ( {model | info = jsonInfo }, Cmd.none)    
    InfoFileDetails1 (Err s) -> ({model | info = toString s}, Cmd.none)

    InfoFileDetails2 (Ok jsonInfo) -> ( {model | xvals = jsonInfo }, Cmd.none)    
    -- InfoFileDetails (Ok jsonInfo) -> ( {model | info = jsonInfo }, Cmd.none)    
    InfoFileDetails2 (Err s) -> ({model | info = toString s}, Cmd.none)

    InfoFileDetails (Ok jsonInfo) -> ( {model | titleDetails = jsonInfo }, Cmd.none)    
    -- InfoFileDetails (Ok jsonInfo) -> ( {model | info = jsonInfo }, Cmd.none)    
    InfoFileDetails (Err s) -> ({model | info = toString s}, Cmd.none)
