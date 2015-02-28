import Html (div, text, a, br)
import Html.Attributes (class, href)

main = div [class "container"] 
  [ 
      text "main div"
    , br [] []
    , a [ class "link", href "http://localhost:3030/foldersDb" ] 
        [ text "foldersDb"] 
  ]
