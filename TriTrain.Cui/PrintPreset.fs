namespace TriTrain.Cui

open TriTrain.Core
open System
open Chessie.ErrorHandling

module PrintPreset =
  let printSkillsTable () =
    let rows =
      Preset.Skill.presetList
      |> List.map (fun (name, oeff) ->
          sprintf "|%s|%s|" name (oeff |> Dump.dumpOEffect)
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
      |> List.map (fun (name, (cond, oeff)) ->
          sprintf "|%s|%s|%s|"
            name
            (cond |> Dump.dumpCond)
            (oeff |> Dump.dumpOEffect)
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
