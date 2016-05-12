module Plates exposing (model, view, update)

import List exposing (head, tail)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

barbellWeight : Float
barbellWeight = 45

model = 45

type alias Plate =
  { weight: Float
  , percentage: Float }

plates : List Plate
plates =
  [ { weight = 45, percentage = 100 }
  , { weight = 35, percentage = 90 }
  , { weight = 25, percentage = 80 }
  , { weight = 10, percentage = 70 }
  , { weight = 5, percentage = 60 }
  , { weight = 2.5, percentage = 50 }
  ]

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
    f weight (List.map (\{weight} -> weight) plates)

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

getPlateWidthPercentage : Float -> Float
getPlateWidthPercentage weight =
  case List.head (List.filter (\plate -> plate.weight == weight) plates) of
    Nothing -> 100
    Just p -> p.percentage

plateHtml : Float -> Html Msg
plateHtml weight =
  li
    [ class "plate"
    , style
      [ ("width", (toString (getPlateWidthPercentage weight)) ++ "%") ]
    ]
    [ text (toString weight) ]

stackOfWeights : Float -> Html Msg
stackOfWeights weight =
  ul
    [ class "plate-container" ]
    (List.map (\n -> plateHtml n) (calculate weight))

stepperButton : Float -> Html Msg
stepperButton val =
  button
    [ onClick (Increment val)
    , class "stepper" ]
    [ text (if val < 0 then (toString val) else "+" ++ (toString val)) ]

type alias Model = Float

view : Model -> Html Msg
view model =
  div
    []
    [ h1
      []
      [ span
          []
          [text (toString model)]
      , text "lbs"
      ]
    , stepperButton -90
    , stepperButton -5
    , stepperButton 5
    , stepperButton 90
    , stackOfWeights model
   ]

type Msg = Increment Float

update : Msg -> Float -> Float
update action model =
  case action of
    Increment val ->
      if (model + val) < barbellWeight
        then barbellWeight
      else model + val
