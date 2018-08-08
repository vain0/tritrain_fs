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
  let internal validateSkills spec =
   
    trial {
      let abils =
        spec
        |> CardSpec.abils
        |> Map.valueSet
        |> Set.toList
        |> List.collect (BatchedQueue.toList)
      let skills =
        spec
        |> CardSpec.skills
        |> Map.valueList
      let kount =
        (abils |> List.length)
        + (skills |> List.collect Skill.toAtomList |> List.length)

      for abil in abils do
        if abil |> Ability.isPreset |> not then
          do! warnf () "'%s' isn't a preset ability." (abil |> fst)

      if skills |> List.forall (Skill.isPresetList) |> not then
        return! () |> warn "Card may have only preset effects."
      if kount > 4 then
        return! () |> warn "A card may have up to 4 effects."
    }

  let validate spec =
    trial {
      do! spec |> CardSpec.status |> Status.validate
      do! spec |> validateSkills
    }

module DeckSpec =
  let validate spec =
    trial {
      for cardSpec in spec |> DeckSpec.cards |> T7.toList do
        do! CardSpec.validate cardSpec
    } |> failOnWarnings
