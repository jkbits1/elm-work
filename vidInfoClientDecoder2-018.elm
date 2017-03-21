import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (field)
import String
import Task exposing (..)
import Time
import JsonBits2 exposing (..)
import List exposing (..)

--  elm make vidInfoClientDecoder2-018a.elm --output=vic2.js

view : Model -> Html Msg
view model =
  div [ 
        -- style (imgStyle height "") 
        class "container"
    ]
    [
      div []
        [ 
          button [ onClick GetFirstFileName ] [ text "send request - first file name" ]
        , button [ onClick GetFileNames ]     [ text "send request - file names" ]
        , button [ onClick GetFileDetails ]   [ text "send request - file details3" ]
        , button [ onClick GetFileDetailsWrapped ]   [ text "send request - file details wrapped" ]        
        ]        
      , div [class "row"] [
          div [class "col-sm-6"] [
            text <| "Count - "            ++ toString model.count
          ]
        -- , div [class "col-sm-6"] [
        --     -- text <| "Info - "             ++ toString model.info
        --   ]
        ]
      , Html.form [] [ 
          div [class "form-group"]
          [
            label [
              for "sortDetails"
            ] [text "Sort Details:"]
          , select [
                class "form-control" 
              , id "sortDetails"
              , onChangeSort
              ]
              [
                option [] [ text <| "number" ]
              , option [] [ text <| "length" ]
              ]
          ]
        , div [ class "form-group" ]
          [
            label [
              for "filterLen"
            ] [text "Filter:"]
          , input 
              [ 
                class "form-control"
              , id "filterLen"
              , placeholder "0.0001"
              , onInput FilterLen 
              ]
              []
          ]
        , div [ class "checkbox" ]
          [
            checkbox Filter "Apply Filter"
          ]
        , div [class "form-group"]
          [
            label [
              for "filesList"
            ] [text "Files List:"]
          , select [ 
                class "form-control"
              , id "filesList"
              , onChangeFileName
              ] 
              <| List.map (\s -> option [] [ text <| s ]) model.fileNames
          -- , text model.currentFileName
          ]
        , div [ class "checkbox" ]
          [
            checkbox ShowJsonErrors "Show Json Errors"
          ]
        ]
      , div [class "firstFileName"]
        [
          text <| "First file name - "  ++ toString model.firstFileName
        ]
      , div [class "fileNames"]
        [
          text "File names - "
        , ul [] <| infoListItems model.fileNames
        , text <| toString model.fileNames
        , text <| ("httpInfo: " ++ model.httpInfo)
        ]

      , div [class "titleDetails"]
        [
          text "Title details - "
        , ul [] <| detailsListItems 
                    <| 
                    List.filter 
                      (\td -> 
                        case model.filter of 
                          True -> td.length > 
                            model.filterLength
                            -- 5
                          False -> True)
                    <| 
                    -- List.sortBy .length
                    List.sortWith 
                      (\td1 td2 -> 
                        case model.sortDetailsByLength of
                          True ->
                            compare td1.length td2.length
                          False ->
                            compare td1.titleNumber td2.titleNumber
                        )
                        model.titleDetails
        ]        
    -- ]
    ]
    
firstSpecific : List TitleDetail -> TitleDetail
firstSpecific details = 
  let m = List.head details
  in 
    case m of 
      Just detail -> detail
      Nothing     -> TitleDetail 0 0.0

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


-- HANDLE RESPONSES

getDetail : String -> Task Http.Error (List String)
getDetail string = succeed [string]

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

main = Html.program { 
    init = init
  , view = view
  , update = update
  , subscriptions = subscriptions 
  }

-- These functions were removed from the Signal package. 
-- They allow the equivalent of map6 and beyond.
-- (<~) : (a -> b) -> Signal a -> Signal b
-- (<~) = Signal.map  
  
-- (~) : Signal (a -> b) -> Signal a -> Signal b
-- (~) funcs args =
--   Signal.map2 (\f v -> f v) funcs args

--infixl 4 ~

model : Model
model = { 
    count = 0
  -- , info = "initial state"
  , firstFileName = ""
  , currentFileName = ""
  , fileNames = []
  , xvals = []
  , titleDetails = [] 
  , sortDetailsByLength = False
  , filter = False
  , showJsonErrors = False
  , filterLength = 0.0001
  , httpInfo = ""
  }

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

respInfo : Http.Response String -> String
respInfo resp = 
             resp.url 
          ++ " " ++ (toString resp.status.code) 
          ++ " " ++ resp.status.message
          ++ " " ++ (toString resp.headers) 
          ++ " " ++ resp.body        

handleError model s = ( {model | httpInfo = s }, Cmd.none)       

handleHttpError model err = 
  case err of 
    Timeout         -> handleError model "timeout"
    NetworkError    -> handleError model "nw error"
    BadUrl s        -> handleError model <| "bad url: " ++ s
    BadStatus resp  -> handleError model <| "bad status: " 
                        ++ (respInfo resp)
    BadPayload s resp  
                    -> handleError model <| "bad payload: " 
                        ++ s ++ " " 
                        ++ (respInfo resp)


update : Msg -> Model -> (Model, Cmd Msg )
update msg model = 
  case msg of 
    NoOp -> (model, Cmd.none)

-- UI settings
    SortDetails sort ->
      case sort of 
        SortByLength ->
          ( {model | count = model.count + 1, sortDetailsByLength = True }, Cmd.none )
        SortByNumber ->
          ( {model | count = model.count + 1, sortDetailsByLength = False }, Cmd.none )

    Filter ->
      ( {model | count = model.count + 1, filter = not model.filter }, Cmd.none )

    FilterLen s ->
      ( {model | count = model.count + 1, 
          filterLength = 
              (\s -> 
                case String.toFloat s of 
                  Ok val -> val
                  Err a  -> 0.0001
              ) s
        }, Cmd.none )

    ShowJsonErrors ->
      -- let
      --   _ = Debug.log "toggle show json errors" model.showJsonErrors 
      -- in
      ( { model | showJsonErrors = not model.showJsonErrors }, Cmd.none )


-- HTTP requests
    GetFirstFileName -> 
      ( {model | count = model.count + 1 }, getFirstFileNameCmd "string" )

    GetFileNames -> 
      ( {model | count = model.count + 1, httpInfo = "reset" }, 
          -- getFileNames "string" 
          getFileNamesMaybeCmd "string" 
          )

    GetFileDetails ->
      ( {model | count = model.count + 1 }, getFileDetailsCmd model.currentFileName )

    GetFileDetailsWrapped  ->
      ( {model | count = model.count + 1 }, getFileDetailsWrappedCmd model.currentFileName )

    GetCurrentFileNameDetails s -> 
      ( {model | count = model.count + 1, currentFileName = s }, getFileDetailsCmd s )
      

-- HTTP responses
    -- triggers http request for first file name details
    InfoFileNames (Ok fileNames) -> 
      let 
        firstFileName = Maybe.withDefault "zzz" <| head fileNames
      in
        ( {model | fileNames = fileNames, currentFileName = firstFileName }, getFileDetailsCmd firstFileName)    
    InfoFileNames (Err err) -> 
      handleHttpError model err

    InfoFileNamesMaybe (Ok fileNames) ->
      let 
        firstFileName = Maybe.withDefault "aaa" <| Maybe.withDefault (Just "xxx") <| head <| 
          filterMaybes model.showJsonErrors fileNames
        _ = Debug.log "show json errors: " (toString model.showJsonErrors)
      in
        ( { model | currentFileName = firstFileName, 
                    fileNames = 
                      List.map (\m -> Maybe.withDefault "dodgy data" m) 
                                      <| filterMaybes model.showJsonErrors fileNames 
          , count = model.count + 3 
          }, Cmd.none)       

    InfoFileNamesMaybe (Err err) -> 
      handleHttpError model err

    InfoFirstFileName (Ok fileName) -> ( {model | firstFileName = fileName }, Cmd.none)    
    InfoFirstFileName (Err _) -> (model, Cmd.none)

    InfoTitleDetails (Ok titleDetails) -> ( {model | titleDetails = titleDetails }, Cmd.none)    
    InfoTitleDetails (Err err) -> handleHttpError model err

filterMaybes showJsonErrors xs = 
  List.filter
  (\x -> 
    case x of 
      Just data -> True
      Nothing -> showJsonErrors
  )
  xs
