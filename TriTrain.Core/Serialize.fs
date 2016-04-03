namespace TriTrain.Core.Serialize

open TriTrain.Core
open TriTrain.Core.Validate
open System
open System.IO
open Chessie.ErrorHandling
open FsYaml

module DeckSpec =
  let deserialize yaml =
    trial {
      let! spec =
        Yaml.customTryLoad<DeckSpec> yaml
        |> Trial.failIfNone "Invalid YAML"
      do! spec |> DeckSpec.validate
      return spec
    }

  let serialize self =
    Yaml.customDump self

  let load path =
    try
      File.ReadAllText(path)
      |> deserialize
    with
    | e -> fail (e.Message)

  let save path spec =
    try
      File.WriteAllText(path, spec |> serialize)
      |> pass
    with
    | e -> fail (e.Message)
