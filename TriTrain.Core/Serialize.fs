namespace TriTrain.Core.Serialize

open TriTrain.Core
open TriTrain.Core.Validate
open System
open System.IO
open Reflection
open Chessie.ErrorHandling
open FsYaml

module Elem =
  let tryParse name =
    DU<Elem>.TryParse(name)
    |> failIfNone (sprintf "Unknown element '%s'." name)

module Status =
  let ofAtAg at ag =
    {
      HP = StatusTotal - (at + ag)
      AT = at
      AG = ag
    }

module OEffect =
  open TriTrain.Core.Preset

  let combineMany skills: option<NamedOEffect> =
    match skills with
    | [] -> None
    | [skill] -> Some skill
    | skills ->
        let (names, skills) = skills |> List.unzip
        let name    = String.Join(" & ", names)
        let skill   = skills |> OEffectList
        in (name, skill) |> Some

  let tryFind name: Result<NamedOEffect, _> =
    OEffect.preset
    |> Map.tryFind name
    |> failIfNone (sprintf "Skill '%s' doesn't exist." name)

  let tryFindList names =
    trial {
      let! skills =
        names
        |> List.map tryFind
        |> Trial.collect
      return
        skills |> combineMany
    }

module CardSpec =
  let ofSrc (src: CardSpecSrc) =
    trial {
      let! elem       = Elem.tryParse (src.Elem)
      let! skillFwd   = src.SkillFwd |> OEffect.tryFindList
      let! skillBwd   = src.SkillBwd |> OEffect.tryFindList
      let abils       = [] // TODO: parse src.Abils
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
