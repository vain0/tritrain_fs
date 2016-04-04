namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open System
open System.Threading
open Chessie.ErrorHandling

module Program =
  let defaultDeckPaths =
    ["l.tritrain_deck"; "r.tritrain_deck"]

  let loadDecks (deckPath1, deckPath2) =
    trial {
      let! deck1 = DeckSpecSrc.load deckPath1
      let! deck2 = DeckSpecSrc.load deckPath2
      let pl1 = PlayerSpec.create (deck1 |> DeckSpec.name) deck1
      let pl2 = PlayerSpec.create (deck2 |> DeckSpec.name) deck2
      return (pl1, pl2)
    }

  let runGameWithObserver observe (pl1, pl2): Game =
    let g = Game.create pl1 pl2
    use o = observe g
    in g |> Game.run

  let showGame deckPaths =
    trial {
      let! plPair = loadDecks deckPaths
      let _ = runGameWithObserver (Broadcaster.observe) plPair
      in ()
    }

  /// 先攻・後攻を固定して2つのデッキを times 回戦わせ、その結果の集計を得る。
  let testBattle times plPair =
    let mutable win  = 0
    let mutable lose = 0
    let mutable draw = 0
    let inc =
      function
      | Win PlLft   -> Interlocked.Increment(& win)
      | Win PlRgt   -> Interlocked.Increment(& lose)
      | Draw        -> Interlocked.Increment(& draw)
      >> ignore
    let gs =
      [ for _ in 1..times ->
          async {
            return runGameWithObserver (ResultNotifier.observe inc) plPair
          }
      ]
      |> Async.Parallel
      |> Async.RunSynchronously
    in (win, lose, draw)

  let testBattleCommand deckPaths =
    trial {
      let! plPair = loadDecks deckPaths
      let (win, lose, draw) = testBattle 10 plPair
      let () = printfn "Win %d - Lose %d - Draw %d" win lose draw
      in ()
    }

  let usage () =
    """
help                    Print this
show deck1 deck2        Show a battle deck1 vs deck2
test deck1 deck2        Simutate battles between deck1 and deck2
"""

  let rec procCommandArgs =
    function
    | [] ->
        match Console.ReadLine() with
        | null | "" -> () |> pass
        | line ->
            line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> procCommandArgs

    | ["show"] ->
        procCommandArgs ("show" :: defaultDeckPaths)
    | "show" :: deckPath1 :: deckPath2 :: _ ->
        showGame (deckPath1, deckPath2)

    | ["test"] ->
        procCommandArgs ("test" :: defaultDeckPaths)
    | "test" :: deckPath1 :: deckPath2 :: _ ->
        testBattleCommand (deckPath1, deckPath2)

    | _ ->
        printfn "%s" (usage ()) |> pass

  [<EntryPoint>]
  let main argv =
    procCommandArgs (argv |> Array.toList)
    |> Trial.runConsoleApp
