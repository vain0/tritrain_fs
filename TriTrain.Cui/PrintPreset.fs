namespace TriTrain.Cui

open TriTrain.Core
open System
open Chessie.ErrorHandling

module PrintPreset =
  let showEffectsCommand () =
    trial {
      do printfn "%s" (Dump.dumpPresetSkillsToMarkdown ())
      do printfn "%s" (Dump.dumpPresetAbilsToMarkdown ())
    }
