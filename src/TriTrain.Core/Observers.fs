[<AutoOpen>]
module TriTrain.Core.Observers

open System

module Logger =
  let observe (kont: GameLog -> unit) (g: Game): IDisposable =
    let acc         = ref []
    let addLog x    = acc := (x :: ! acc)
    let run ()      = (! acc) |> List.rev |> kont
    in
      g
      |> Game.asObservable
      |> Observable.subscribeAll addLog ignore run
