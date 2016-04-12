namespace TriTrain.Core.Test

open Persimmon
open TriTrain.Core
open TriTrain.Core.Random

module DeckSpec =
  let ofCard name card: DeckSpec =
    {
      Name          = name
      Cards         = T7.replicate card
    }

module PlayerSpec =
  let replicateMan name card =
    {
      Name          = name
      Deck          = DeckSpec.ofCard name card
    }

module Player =
  /// 盤面にカードを1体だけ置いた状態のプレイヤー
  let ofSingleCard plId vx card =
    {
      PlayerId      = plId
      Spec          = PlayerSpec.replicateMan "" (card |> Card.spec)
      Deck          = []
      Board         = [(vx, card |> Card.cardId)] |> Map.ofList
      Trash         = Set.empty
    }

module Game =
  /// 互いに前列に同じカードを置いているだけのゲーム
  let ofSingleCards plId spec card =
    {
      PlLft         = Player.ofSingleCard PlLft Fwd card
      PlRgt         = Player.ofSingleCard PlRgt Fwd card
      CardMap       = [(card |> Card.cardId, card)] |> Map.ofList
      Turn          = 0
      Triggered     = []
      Events        = Observable.Source()
    }
