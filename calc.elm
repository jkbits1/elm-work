import Signal
import String
import Char
import Graphics.Element (flow, down, right, layers, container, middle, spacer, color, bottomRightAt, absolute, Element)
import Graphics.Input as Input
import Color (black, grey, white, rgba, Color)
--import Text (fromString)
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
  Digit String | Decimal | Add | Mult | Equals | Clear
  
type State =
    Start Number 
  | Operator Float (Float -> Float -> Float) Number

type alias Number =
  { negative: Bool, string: String, percentage: Int}
  
makeNumber : String -> Number
makeNumber s = { negative = False, string = s, percentage = 0 }
  
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
    
    Mult -> operator (*) state
    
    Equals -> Start (makeNumber (toString (equals state)))
    
    Clear -> clear state
    
modifyNumber : (Number -> Number) -> State -> State
modifyNumber f state =
  case state of
    Start n -> Start (f n)
    
    Operator n op m -> Operator n op (f m)
    
appendIf : (Number -> Bool) -> String -> Number -> Number    
appendIf isOkay str number =
  if isOkay number
  then { number | string <- number.string ++ str }
  else number

clear : State -> State
clear state =
  case state of
    Start n -> Start zero
    
    Operator n op m -> 
      if m == zero then Start zero else Operator n op zero
    
operator : (Float -> Float -> Float) -> State -> State
operator op state =
  case state of
    Start n -> Operator (numberToFloat n) op zero
    
    Operator n _ m -> 
      Operator (if m == zero then n else equals state) op zero
    
buttonSize : number
buttonSize = 80
    
calculator : State -> Element
calculator state =
  let pos = bottomRightAt (absolute 10) (absolute 10)
  in
    flow down 
      [
          color black << 
            container (4 * buttonSize) (buttonSize + 40) pos <| 
            screen 0.6 (toString (displayNumber state))
        , buttons
      ]
    
screen : Float -> String -> Element
screen size text =
  txt size white text
  
displayNumber : State -> Float
displayNumber state =
  case state of
    Start n -> numberToFloat n
    
    Operator n op m -> if m == zero then n else numberToFloat m
    
buttons : Element
buttons = 
  flow down
    [
        flow right [topOp Clear "C"]
      , flow right [number "1", number "2", number "3", rightOp Add "+"]
      , flow right [number "4", number "5", number "6", rightOp Mult "*"]
      , flow right [numberZero, rightOp Equals "="]
    ]

button : Int -> Command -> String -> Element
button w command name =
--  let btn alpha = 
--        layers 
--          [
--            container 15 15 middle (txt 0.3 black)
--          ]
--  in Input.customButton (Signal.send commandChnl command) 
--  in  customButton (Signal.send commandChnl command) 
  Input.customButton (Signal.send commandChnl command) 
                        (btn w 0 name) (btn w 0.05 name) (btn w 0.1 name)

btn : Int -> Float -> String -> Element
btn w alpha name = 
  let -- w = buttonSize
      h = buttonSize 
  in
        layers 
          [
--            container 15 15 middle (txt 0.3 black name)
              container w h middle (txt 0.3 grey name)
            , color (rgba 0 0 0 alpha) (spacer w h)
          ]                        

topOp : Command -> String -> Element
topOp command name = 
  button buttonSize command name
  
rightOp : Command -> String -> Element  
rightOp command name =
  button buttonSize command name
  
number : String -> Element
number n = numButton (Digit n) n

numberZero : Element
numberZero = 
  let n = "0"
  in 
    numWideButton (Digit n) n

numButton : Command -> String -> Element
numButton = button buttonSize 

numWideButton : Command -> String -> Element
numWideButton = button (buttonSize * 2)

txt : Float -> Color -> String -> Element
txt p clr string =
  Text.fromString string
    |> Text.color clr
    |> Text.leftAligned
        
equals : State -> Float
equals state = 
  case state of 
    Start n -> numberToFloat n
    
    Operator n op m -> op n (if m == zero then n else numberToFloat m)