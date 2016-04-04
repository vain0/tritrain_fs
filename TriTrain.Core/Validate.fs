namespace TriTrain.Core.Validate

open System
open TriTrain.Core
open TriTrain.Core.Preset
open Chessie.ErrorHandling

module Status =
  let validate status =
    trial {
      if status |> Status.hp <= 0 then
        do! warnf () "HP must be positive: %d." (status |> Status.hp)
      if status |> Status.at < 0 then
        do! warnf () "AT mustn't be negative: %d." (status |> Status.at)
      if status |> Status.ag |> isInInterval 0 (MaxDefaultAG + 1) |> not then
        do! warnf () "AG must be in range 0-%d: %d." MaxDefaultAG (status |> Status.ag)
      if status |> Status.total > StatusTotal then
        do!
          warnf () "Status total mustn't be over %d: %d."
            StatusTotal (status |> Status.total)
    }

module CardSpec =
  let internal validateAbils spec =
    trial {
      if spec |> CardSpec.abils |> List.isEmpty |> not then
        return! () |> warn "Card may have no abilities yet."
    }

  let internal validateSkills spec =
    trial {
      let skills =
        spec
        |> CardSpec.skills
        |> Map.valueSet
        |> Set.toList
        |> List.map snd // discard names
      if skills |> List.forall (OEffect.isPreset) |> not then
        return! () |> warn "Card may have only preset effects."
      if skills |> List.collect OEffect.toPresetList |> List.length > 4 then
        return! () |> warn "A card may have up to 4 effects."
    }

  let validate spec =
    trial {
      do! spec |> CardSpec.status |> Status.validate
      do! spec |> validateAbils
      do! spec |> validateSkills
    }

module DeckSpec =
  let validate spec =
    trial {
      for cardSpec in spec |> DeckSpec.cards |> T7.toList do
        do! CardSpec.validate cardSpec
    } |> failOnWarnings
