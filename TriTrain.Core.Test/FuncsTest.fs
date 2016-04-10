module TriTrain.Core.Test.FuncsTest

open Persimmon.Syntax.UseTestNameByReflection
open Persimmon
open Persimmon.Dried

let ``rev . rev = id`` =
  Prop.forAll (Arb.list Arb.int) (fun xs ->
    List.rev (List.rev xs) = xs
  )

let ``check property`` =
  property "" {
    apply ``rev . rev = id``
  }
