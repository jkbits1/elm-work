module LightsModule exposing (..)

type Colour = Red | Amber | Green
type alias ColourSet = List Colour

type alias LightState = { cols : ColourSet }

type alias NumberedLightState = (LightState, Int)

stop : ColourSet
stop  = [Red]
ready = [Red, Amber]
go    = [Green]
slow  = [Amber]

states : List LightState
states = 
  [ { cols = stop }
  , { cols = ready }  
  , { cols = go }
  , { cols = slow }
  ]
  
zip = List.map2 (,)  

stateNumbered : List NumberedLightState
stateNumbered = zip states [1.. List.length states]

getNlsNum : NumberedLightState -> Int
getNlsNum nls = snd nls

stateByNum n = 
  List.map (.cols)
    <| List.map (fst)
      <| List.filter (\nls -> getNlsNum(nls) == n) stateNumbered

safeStateByNum n = Maybe.withDefault stop <| List.head <| stateByNum n


