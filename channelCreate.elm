-- NOTE: code comes from Signal.send package example

--import Signal (channel)
import Signal 
import Html
import Html.Events

type Update = NoOp | Add Int | Remove Int

updates : Signal.Channel Update
updates = Signal.channel NoOp

addButton : Html.Html
addButton = Html.button
  [ Html.Events.onClick (Signal.send updates (Add 1))]
  [ Html.text "Add 1" ]

-- this bit, from Signal package example doesn't work
--main : Signal Element
--main = Signal.map 
--  (view updates)
--  (foldp step initialState (subscribe updates))