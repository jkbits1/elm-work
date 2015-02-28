import Signal
import String
import Char
import Graphics.Element (flow, down, right, layers, container, middle, Element)
import Graphics.Input as Input
import Color (black, grey, Color)
import Text

main : Signal Element
main = 
--  Signal.subscribe commandChnl
--    |> Signal.foldp update (Start zero)
--    |> Signal.map calculator 
    Signal.map calculator (Signal.foldp update (Start zero) (Signal.subscribe commandChnl))
  
     

commandChnl : Signal.Channel Command
commandChnl = Signal.channel Clear

type Command =
  Digit String | Decimal | Add | Clear
  
type State =
    Start Number 
  | Operator Float (Float -> Float -> Float) Number

type alias Number =
  { negative: Bool, string: String, percentage: Int}
  
zero : Number
zero = { negative = False, string = "", percentage = 0 }

numberToFloat : Number -> Float
numberToFloat number =
  let neg = if number.negative then -1 else 1
      exp = 100 ^ toFloat number.percentage
  in
      case String.toFloat number.string of
        Ok n -> n * neg / exp
        Err _ -> 0
  
update : Command -> State -> State
update command state =
  case command of
    Digit digit ->
      let isShort n = String.length 
                        (String.filter Char.isDigit n.string) < 10
      in modifyNumber (appendIf isShort digit) state
      
    Decimal ->
      let noDot n = String.all ((/=) '.') n.string
      in modifyNumber (appendIf noDot ".") state
      
    Add -> operator (+) state
    
    Clear -> clear state
    
modifyNumber : (Number -> Number) -> State -> State
modifyNumber f state =
  case state of
    Start n -> Start (f n)
    
appendIf : (Number -> Bool) -> String -> Number -> Number    
appendIf isOkay str number =
  if isOkay number
  then { number | string <- number.string ++ str }
  else number

clear : State -> State
clear state =
  case state of
    Start n -> Start zero
    
operator : (Float -> Float -> Float) -> State -> State
operator op state =
  case state of
    Start n -> Operator (numberToFloat n) op zero
    
calculator : State -> Element
calculator state =
  flow down 
    [
      buttons
    ]
    
buttons : Element
buttons = 
  flow down
    [
      flow right [topOp Clear "C"]
      , flow right [number "1", number "2"]
    ]

button : Command -> String -> Element
button command name =
--  let btn alpha = 
--        layers 
--          [
--            container 15 15 middle (txt 0.3 black)
--          ]
--  in Input.customButton (Signal.send commandChnl command) 
--  in  customButton (Signal.send commandChnl command) 
  Input.customButton (Signal.send commandChnl command) 
                        (btn 0 name) (btn 0.05 name) (btn 0.1 name)

btn : Float -> String -> Element
btn alpha name = 
  let w = 25
      h = 25 
  in
        layers 
          [
--            container 15 15 middle (txt 0.3 black name)
            container w h middle (txt 0.3 grey name)
          ]
                        

topOp : Command -> String -> Element
topOp command name = 
  button command name
  
number : String -> Element
number n = numButton (Digit n) n

numButton : Command -> String -> Element
numButton = button 

txt : Float -> Color -> String -> Element
txt p clr string =
  Text.fromString string
    |> Text.leftAligned
    
  