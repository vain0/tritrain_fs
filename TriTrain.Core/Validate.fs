namespace TriTrain.Core.Validate

open System
open TriTrain.Core
open TriTrain.Core.Preset
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

module CardSpec =
  let internal validateName spec =
    trial {
      if spec |> CardSpec.name |> String.isNamey |> not then
        return! fail "Card name is invalid."
    }

  let internal validateAbils spec =
    trial {
      if spec |> CardSpec.abils |> List.isEmpty |> not then
        return! fail "Card may have no abilities yet."
    }

  let internal validateSkills spec =
    trial {
      let skills =
        spec
        |> CardSpec.skills
        |> Map.valueSet
        |> Set.toList
        |> List.collect (fun (_, oeff) -> oeff |> OEffect.toList)
      if skills |> List.forall (OEffect.isPreset) |> not then
        return! fail "Card may have only preset effects."
      if skills |> List.length > 4 then
        return! fail "A card may have up to 4 effects."
    }

  let validate spec =
    trial {
      do! spec |> validateName
      do! spec |> CardSpec.status |> Status.validate
      do! spec |> validateAbils
      do! spec |> validateSkills
    }

module DeckSpec =
  let validate spec =
    trial {
      if spec |> DeckSpec.name |> String.isNamey |> not then
        return! fail "Deck name is invalid."
      for cardSpec in spec |> DeckSpec.cards |> T7.toList do
        do! CardSpec.validate cardSpec
    }
