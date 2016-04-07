namespace TriTrain.Cui

open TriTrain.Core
open System
open Chessie.ErrorHandling

module PrintPreset =
  let printSkillsTable () =
    let rows =
      Preset.Skill.presetList
      |> List.map (fun (name, oeffs) ->
          sprintf "|%s|%s|" name (oeffs |> Dump.dumpOEffectList)
          )
      |> String.concat (Environment.NewLine)
    let header =
        """
## 行動(Skills)
|Name|Text|
|:---|:---|
"""
    do printfn "%s" (header + rows)

  let printAbilsTable () =
    let rows =
      Preset.Ability.presetList
      |> List.map (fun (name, (cond, oeffs)) ->
          sprintf "|%s|%s|%s|"
            name
            (cond |> Dump.dumpCond)
            (oeffs |> Dump.dumpOEffectList)
          )
      |> String.concat (Environment.NewLine)
    let header =
        """
## 能力(Abilities)
|Name|Cond|Text|
|:---|:---|:---|
"""
    do printfn "%s" (header + rows)

  let showEffectsCommand () =
    trial {
      do printSkillsTable ()
      do printAbilsTable ()
    }
