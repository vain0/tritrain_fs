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

module Skill =
  let combineMany skills: option<Skill> =
    match skills with
    | [] -> None
    | [skill] -> Some skill
    | skills ->
        let (names, skills) = skills |> List.unzip
        let name    = String.Join(" & ", names)
        let skill   = skills |> OEffectList
        in (name, skill) |> Some

  let tryFind name: Result<Skill, _> =
    Skill.preset
    |> Map.tryFind name
    |> failfIfNone "Skill '%s' doesn't exist." name

  let tryFindList names =
    trial {
      let! skills =
        names
        |> List.map tryFind
        |> Trial.collect
      return
        skills |> combineMany
    }

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

module CardSpec =
  let ofSrc (src: CardSpecSrc) =
    trial {
      let! elem       = Elem.tryParse (src.Elem)
      let! skillFwd   = src.SkillFwd |> Skill.tryFindList
      let! skillBwd   = src.SkillBwd |> Skill.tryFindList
      let! abils      = src.Abils |> Ability.ofSrc
      let skills =
        [
          (FwdRow, skillFwd)
          (BwdRow, skillBwd)
        ]
        |> Map.ofList
        |> Map.choose (fun k -> id)
      return
        {
          Name      = src.Name
          Status    = Status.ofAtAg (src.AT) (src.AG)
          Elem      = elem
          Abils     = abils
          Skills    = skills
        }
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
