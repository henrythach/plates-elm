import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp

main =
  StartApp.start { model = model, view = view, update = update }

model = 45

plates : List Float
plates = [45, 35, 25, 10, 5, 2.5]

{-
function calculate(weight) {
  let results = [];

  if (weight < BARBELL_WEIGHT || weight % 5 !== 0) {
    return results;
  }

  weight -= BARBELL_WEIGHT;
  weight /= 2;
  while (weight > 0) {
    for (let plate of PLATES) {
      if (weight >= plate) {
        weight -= plate;
        results.push(plate);
        break;
      }
    }
  }

  return results;
}
-}

calculate : Int -> List Float
calculate weight =
  if weight < 45 || weight % 5 /= 0 then
    []
  else
    calculate' (weight) []

calculate' : Int -> List Float -> List Float
calculate' weight stack =
  if weight == 0 then
    stack
  else
    calculate' (weight - 5) (stack ++ [5])

view address model =
  div
    []
    [ h1
      []
      [ span
        [id "targetWeightSpan"]
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
  = Increment Int
  | Decrement Int

update action model =
  case action of
    Increment val -> model + val
    Decrement val ->
      if (model - val) < 45
        then 45
        else model - val
