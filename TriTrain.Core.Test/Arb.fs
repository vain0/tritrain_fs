namespace TriTrain.Core.Test

open TriTrain.Core
open Persimmon.Dried
open Persimmon.Dried.ArbitrarySyntax

module Arb =
  let oneOf xs =
    {
      Gen             = xs |> Seq.map Gen.constant |> Gen.oneOf
      Shrinker        = Shrink.shrinkAny
      PrettyPrinter   = Pretty.prettyAny
    }

  let name = Arb.nonNull Arb.string

  let status =
    arbitrary {
      let! ag = seq { 0..50 } |> oneOf
      let! at = seq { 0..(StatusTotalMax - ag) } |> oneOf
      return Status.ofAtAg at ag
    }
