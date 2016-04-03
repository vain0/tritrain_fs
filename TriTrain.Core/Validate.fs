namespace TriTrain.Core.Validate

open TriTrain.Core
open Chessie.ErrorHandling

module Status =
  let validate status =
    trial {
      if status |> Status.hp <= 0 then
        return! fail "HP must be positive."
      if status |> Status.at < 0 then
        return! fail "AT mustn't be negative."
      if status |> Status.ag |> isInInterval 0 51 |> not then
        return! fail "AG must be in range 0-50."
      if status |> Status.total > 300 then
        return! fail "Status total mustn't over 300."
    }
