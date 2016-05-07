import List exposing (head, tail)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp

main =
  StartApp.start
    { model = model
    , view = view
    , update = update }

barbellWeight : Float
barbellWeight = 45

plates : List Float
plates = [45, 35, 25, 10, 5, 2.5]

getClosestPlateFor : Float -> Float
getClosestPlateFor weight =
  let f weight plates =
    case (List.head plates) of
      Nothing ->
        0
      Just plate ->
        if (weight >= plate) then
          plate
        else
          f weight (List.drop 1 plates)
  in
    f weight plates

getStackOfWeights : Float -> List Float -> List Float
getStackOfWeights weight stack =
  if weight == 0 then
    stack
  else
    let f plate =
      getStackOfWeights (weight - plate) (stack ++ [plate])
    in
      f (getClosestPlateFor weight)

calculate : Float -> List Float
calculate weight =
  if weight < 45 || weight % 5 /= 0 then
    []
  else
    getStackOfWeights ((weight - 45) / 2) []

stackOfWeights : Float -> Html
stackOfWeights weight =
  ul
    []
    (List.map
      (\n -> li [] [text (toString n)])
      (calculate weight)
    )


model : Float
model = barbellWeight

stepperButton : Float -> Signal.Address Action -> Html
stepperButton val address =
  button
    [ onClick address (Increment val)
    , class "stepper" ]
    [ text (if val < 0 then (toString val) else "+" ++ (toString val)) ]

view address model =
  div
    []
    [ h1
      []
      [ span
          []
          [text (toString model)]
      , text "lbs"
      ]
    , stepperButton -90 address
    , stepperButton -5 address
    , stepperButton 5 address
    , stepperButton 90 address
    , stackOfWeights model
   ]

type Action
  = Increment Float

update : Action -> Float -> Float
update action model =
  case action of
    Increment val ->
      if (model + val) < barbellWeight
        then barbellWeight
      else model + val
