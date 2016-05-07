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

model : Float
model = barbellWeight -- initial value

plates : List Float
plates = [45, 35, 25, 10, 5, 2.5]

getClosestPlateFor : Float -> Float
getClosestPlateFor weight =
  let iterate weight plates =
    case (List.head plates) of
      Nothing ->
        0
      Just plate ->
        if (weight >= plate) then
          plate
        else
          iterate weight (List.drop 1 plates)
  in
    iterate weight plates

calculate : Float -> List Float
calculate weight =
  if weight < 45 || weight % 5 /= 0 then
    []
  else
    let iterate weight stack =
      if weight == 0 then
        stack
      else
        iterate (weight - (getClosestPlateFor weight)) (stack ++ [(getClosestPlateFor weight)])
    in
      iterate ((weight - 45) / 2) []

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
    , button
        [ class "stepper"
        , onClick address (Decrement 90) ]
        [ text "-90" ]
    , button
        [ class "stepper"
        , onClick address (Decrement 5) ]
        [ text "-5"]
    , button
        [ class "stepper"
        , onClick address (Increment 5) ]
        [ text "+5" ]
    , button
        [ class "stepper"
        , onClick address (Increment 90) ]
        [ text "+90" ]
    , stackOfWeights model
   ]

stackOfWeights weight =
  ul
    []
    (List.map
      (\n -> li [] [text (toString n)])
      (calculate weight)
    )

type Action
  = Increment Float
  | Decrement Float

update : Action -> Float -> Float
update action model =
  case action of
    Increment val -> model + val
    Decrement val ->
      if (model - val) < barbellWeight
        then barbellWeight
        else model - val
