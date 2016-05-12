import Html.App as Html
import Plates exposing (model, view, update)

main =
  Html.beginnerProgram
    { model = 45
    , view = view
    , update = update }
