namespace TriTrain.Core.Test

open Persimmon
open Persimmon.Dried
open Chessie.ErrorHandling
open TriTrain.Core
open TriTrain.Core.Preset
open TriTrain.Core.Serialize

module Arb =
  let ofGen gen =
    {
      Gen             = gen
      Shrinker        = Shrink.shrinkAny
      PrettyPrinter   = Pretty.prettyAny
    }

  let oneOf (xs: #seq<'x>): Arbitrary<'x> =
    xs |> Seq.map Gen.constant |> Gen.oneOf |> ofGen

  let name =
    Arb.nonNull Arb.string

  let elem =
    oneOf Elem.all

  let abils n =
    Ability.presetList |> Gen.pick n |> ofGen

  let skillNames n =
    gen {
      let! ss = Skill.presetList |> Gen.pick n |> ofGen
      return ss |> Seq.toList |> List.map (SkillAtom >> Skill.name)
    }

  let cardSpecSrc =
    gen {
      let! countAbils     = [0; 1] |> oneOf
      let! countFwds      = [0..(4 - countAbils)] |> oneOf
      let  countBwds      = 4 - (countAbils + countFwds)
      let! name       = name
      let! elem       = elem
      let! ag         = seq { 0..MaxDefaultAG } |> oneOf
      let! at         = seq { 0..(StatusTotal - ag) } |> oneOf
      let! abils      = abils countAbils
      let! skillFwd   = skillNames countFwds
      let! skillBwd   = skillNames countBwds
      let  skills     =
        [
          for s in skillFwd do yield (FwdRow, s)
          for s in skillBwd do yield (BwdRow, s)
        ] |> Map.ofList
      return
        {
          Name        = name
          Elem        = elem |> Elem.toString
          AT          = at
          AG          = ag
          Abils       = abils |> Seq.toList |> List.map Ability.name
          SkillFwd    = skillFwd
          SkillBwd    = skillBwd
        }
    }

  let cardSpec =
    gen {
      let! src = cardSpecSrc
      return src |> CardSpec.ofSrc |> Trial.returnOrFail
    }

  let deckSpec =
    gen {
      let! name = name
      let! cards = Gen.listOfLength 7 cardSpec
      return
        ({
          Name      = name
          Cards     = cards |> T7.ofList |> Option.get
        }: DeckSpec)
    } |> ofGen
