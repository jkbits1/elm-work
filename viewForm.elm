import Signal
import Graphics.Input.Field as Field
import Graphics.Element (Element, flow, down)
import Text

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
        Text.leftAligned <| Text.fromString "123"
--      , Field.field Field.defaultStyle ((Signal.send updateChnl) << Name) "empty" model.name 
      , Field.field Field.defaultStyle (contentToUpdate (Signal.send updateChnl) Name) "name..." model.name 
      , Field.field Field.defaultStyle (contentToUpdate (Signal.send updateChnl) Email) "email..." model.email 
    ]

main : Signal Element
main = Signal.map view model

--NOTE: we create a fn that given a Content, converts this to an Update, and then converts the update to a Message
--NOTE: both definitions work
--contentToUpdate : (Update -> Signal.Message) -> (Field.Content -> Update) -> Field.Content -> Signal.Message
contentToUpdate : (u -> m) -> (c -> u) -> c -> m
--NOTE: both lines work
contentToUpdate g f = \x -> g (f x)
--contentToUpdate g f = g << f

port redirect : Signal String
port redirect = 
  Signal.map2 toUrl (Signal.subscribe updateChnl) model
    |> Signal.keepIf (not << String.isEmpty) "" 

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

isEmpty : String -> Bool
isEmpty content = 
  String.isEmpty content.string
  

