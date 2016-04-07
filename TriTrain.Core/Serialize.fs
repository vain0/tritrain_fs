namespace TriTrain.Core.Serialize

open TriTrain.Core
open TriTrain.Core.Validate
open TriTrain.Core.Preset
open System
open System.IO
open Reflection
open Chessie.ErrorHandling
open FsYaml

module Elem =
  let tryParse name =
    DU<Elem>.TryParse(name)
    |> failfIfNone "Unknown element '%s'." name

  let toString (elem: Elem): string =
    DU<Elem>.StringizeUnitCase(elem)

module Skill =
  let tryFind name: Result<Skill, _> =
    Skill.preset
    |> Map.tryFind name
    |> failfIfNone "Skill '%s' doesn't exist." name

  let ofSrc names =
    trial {
      let! skills =
        names
        |> List.map tryFind
        |> Trial.collect
      return
        skills |> Skill.ofList
    }

  let toSrcOf row (skills: Map<Row, Skill>): list<Name> =
    match skills |> Map.tryFind row with
    | None -> []
    | Some skill -> skill |> Skill.toNameList

module Ability =
  let ofSrc (src: AbilitySrc) =
    trial {
      let! abils =
        src
        |> List.map (fun name ->
            Ability.preset |> Map.tryFind name
            |> failfIfNone "Ability '%s' doesn't exist." name
            )
        |> Trial.collect
      return
        abils |> List.fold (fun m abil ->
            m |> Ability.add abil
            ) Map.empty
    }

  let toSrc (abil: Ability) =
    abil |> Ability.name

module CardSpec =
  let ofSrc (src: CardSpecSrc) =
    trial {
      let! elem       = Elem.tryParse (src.Elem)
      let! skillFwd   = src.SkillFwd |> Skill.ofSrc
      let! skillBwd   = src.SkillBwd |> Skill.ofSrc
      let! abils      = src.Abils |> Ability.ofSrc
      let skills =
        [
          (FwdRow, skillFwd)
          (BwdRow, skillBwd)
        ]
        |> Map.ofList
      return
        {
          Name      = src.Name
          Status    = Status.ofAtAg (src.AT) (src.AG)
          Elem      = elem
          Abils     = abils
          Skills    = skills
        }
    }

  let toSrc (cspec: CardSpec) =
    {
      Name        = cspec |> CardSpec.name
      AT          = cspec |> CardSpec.status |> Status.at
      AG          = cspec |> CardSpec.status |> Status.ag
      Elem        = cspec |> CardSpec.elem |> Elem.toString
      Abils       = cspec |> CardSpec.abilList |> List.map (Ability.toSrc)
      SkillFwd    = cspec |> CardSpec.skills |> Skill.toSrcOf FwdRow
      SkillBwd    = cspec |> CardSpec.skills |> Skill.toSrcOf BwdRow
    }

module DeckSpec =
  let ofSrc (src: DeckSpecSrc): Result<DeckSpec, _> =
    trial {
      let! cards =
        src.Cards
        |> T7.toList
        |> List.map CardSpec.ofSrc
        |> Trial.collect
      let! cards =
        cards 
        |> T7.ofList
        |> failIfNone "A deck must consist of exactly 7 cards."
      return
        {
          Name      = src.Name
          Cards     = cards
        }
    }

  let toSrc (dspec: DeckSpec): DeckSpecSrc =
    {
      Name        = dspec |> DeckSpec.name
      Cards       = dspec |> DeckSpec.cards |> T7.map (CardSpec.toSrc)
    }

module DeckSpecSrc =
  let deserialize yaml =
    trial {
      let! src =
        Yaml.myTryLoad<DeckSpecSrc> yaml
        |> Trial.mapFailure
            (fun es -> es |> List.map (fun e -> e.Message))
      let! spec = src |> DeckSpec.ofSrc
      do! spec |> DeckSpec.validate
      return spec
    }

  let serialize (self: DeckSpecSrc) =
    Yaml.dump self

  let load path =
    try
      File.ReadAllText(path)
      |> deserialize
    with
    | e -> fail (e.Message)

  let save path (spec: DeckSpecSrc) =
    try
      File.WriteAllText(path, spec |> serialize)
      |> pass
    with
    | e -> fail (e.Message)
