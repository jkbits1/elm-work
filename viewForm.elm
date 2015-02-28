import Signal
import Graphics.Input.Field as Field
import Graphics.Input as Input
import Graphics.Element (Element, flow, down, spacer)
import Text
import String
import List

type Update = 
    Name    Field.Content
  | Email   Field.Content
  | Submit

type alias Model = 
  {
      name          : Field.Content
    , email         : Field.Content
    , sendAttempts  : Int
  }
  
update : Update -> Model -> Model
update updt model =
  case updt of
    Name content ->
      { model | name <- content}
    
    Email content ->
      { model | email <- content }
      
    Submit ->
    { model | sendAttempts <- model.sendAttempts + 1 }
      
emptyModel : Model
emptyModel =
  {
      name = Field.noContent
    , email = Field.noContent
    , sendAttempts = 0
  }
    

updateChnl : Signal.Channel Update
updateChnl = Signal.channel Submit


--NOTE - work to understand this next


model : Signal Model
--model = Signal.subscribe updateChnl
--        |> Signal.foldp update emptyModel
model = Signal.foldp update emptyModel (Signal.subscribe updateChnl)

view : Model -> Element
view model =
  flow down 
    [
        Text.leftAligned <| Text.fromString "Redirect Form"
--      , Field.field Field.defaultStyle ((Signal.send updateChnl) << Name) "empty" model.name 
--      , Field.field Field.defaultStyle 
--        (contentToMessage (Signal.send updateChnl) Name) "name..." 
--        model.name 
--      , Field.field Field.defaultStyle 
--        (contentToMessage (Signal.send updateChnl) Email) "email..."       
--        model.email 
      , viewField Name "name..." model.name
      , viewField Email "email..." model.email
      , viewErrors model
      , Input.button (Signal.send updateChnl Submit) "Submit"
    ]
    
viewField : (Field.Content -> Update) -> String -> Field.Content -> Element
viewField contentToUpdate placeholder content = 
  Field.field Field.defaultStyle 
    (contentToMessage contentToUpdate (Signal.send updateChnl))
    placeholder content

main : Signal Element
main = Signal.map view model

--NOTE: we create a fn that given a Content, converts this to an Update, 
--      and then converts the update to a Message
--NOTE: both definitions work
contentToMessage : (Field.Content -> Update) ->
                    (Update -> Signal.Message) -> 
                    Field.Content -> Signal.Message
--contentToMessage : (c -> u) -> (u -> m) -> c -> m
--NOTE: both lines work
contentToMessage f g = \x -> g (f x)
--contentToMessage f g = g << f

port redirect : Signal String
port redirect = 
--  Signal.map2 toUrl (Signal.subscribe updateChnl) model
--    |> Signal.keepIf (not << String.isEmpty) "" 
  Signal.keepIf (not << String.isEmpty) "" 
    (Signal.map2 toUrl (Signal.subscribe updateChnl) model)

toUrl : Update -> Model -> String
toUrl update model =
  if not (List.isEmpty (getErrors model))
    then ""
    else
      case update of
        Submit ->
              "/login?name="  ++ model.name.string
          ++  "&email"        ++ model.email.string
          
        _ -> ""
        
getErrors : Model -> List String
getErrors {name, email} =
  List.filterMap checkForError 
    (checks 
      { name = name, email = email, sendAttempts = 0 }
    )

isEmpty : Field.Content -> Bool
isEmpty content = 
  String.isEmpty content.string
  
checkForError : (Bool, String) -> Maybe String
checkForError (err, msg) = 
  if err
    then Just msg
    else Nothing
  
checks : Model -> List (Bool, String)  
checks { name, email } = 
  [
      (isEmpty name,  "Need name")
    , (isEmpty email, "Need email")
  ]
  
viewErrors : Model -> Element
viewErrors model =
--  errors : Model -> List String
  let errors = 
    if model.sendAttempts > 0
      then
        getErrors model
      else 
        []
  in
    flow down
      [
        if List.isEmpty errors
          then spacer 0 0
          else flow down (List.map viewError errors)           
      ]

viewError : String -> Element
viewError = (\errorMsg -> Text.centered <| Text.fromString errorMsg)
--viewError msg = Text.centered (Text.fromString msg)
    