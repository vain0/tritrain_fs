namespace TriTrain.Cui

open TriTrain.Core
open TriTrain.Core.Serialize
open System
open System.Threading
open Chessie.ErrorHandling

module Program =
  let defaultDeckPaths =
    ["l.trtrdeck"; "r.trtrdeck"]

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

  let stringizeResult (win, lose, draw) =
    sprintf "Win %d - Lose %d - Draw %d" win lose draw

  let testBattleCommand deckPaths =
    trial {
      let! plPair = loadDecks deckPaths
      let result = testBattle 10 plPair
      let () = printfn "%s" (stringizeResult result)
      in ()
    }

  /// 総当たりでテストバトルを行う
  let roundRobin times pls =
    Seq.product pls pls
    |> Seq.map (fun plPair -> async {
        return (plPair, testBattle 10 plPair)
        })
    |> Async.Parallel
    |> Async.RunSynchronously

  // 総当たりの結果をリスト形式で表示する
  let printRoundRobinResultsAsList results =
    for ((pl1, pl2), result) in results do
      printfn "%s vs %s: %s"
        (pl1 |> PlayerSpec.name)
        (pl2 |> PlayerSpec.name)
        (stringizeResult result)

  let roundRobinCommand deckPaths =
    trial {
      let! decks =
        deckPaths
        |> List.map (DeckSpecSrc.load)
        |> Trial.collect
      let pls =
        [ for deck in decks ->
            PlayerSpec.create (deck |> DeckSpec.name) deck ]
      let results =
        roundRobin 10 pls
      do printRoundRobinResultsAsList results
    }

  let printSkillsTable () =
    let rows =
      Preset.Skill.presetList
      |> List.map (fun (name, oeff) ->
          sprintf "|%s|%s|" name (oeff |> Dump.dumpOEffect)
          )
      |> String.concat (Environment.NewLine)
    let header =
        """
## 行動(Skills)
|Name|Text|
|:---|:---|
"""
    do printfn "%s" (header + rows)

  let printAbilsTable () =
    let rows =
      Preset.Ability.presetList
      |> List.map (fun (name, (cond, oeff)) ->
          sprintf "|%s|%s|%s|"
            name
            (cond |> Dump.dumpCond)
            (oeff |> Dump.dumpOEffect)
          )
      |> String.concat (Environment.NewLine)
    let header =
        """
## 能力(Abilities)
|Name|Cond|Text|
|:---|:---|:---|
"""
    do printfn "%s" (header + rows)

  let showEffectsCommand () =
    trial {
      do printSkillsTable ()
      do printAbilsTable ()
    }

  let usage () =
    """
help                    Print this
show deck1 deck2        Show a battle deck1 vs deck2
test deck1 deck2        Simutate battles between deck1 and deck2
rr   decks...           Simulate round-robin tournament with decks
effs                    Show preset effects
"""

  let (|RoundRobin|_|) =
    function
    | "rr" | "round-robin" -> Some ()
    | _ -> None

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

    | [RoundRobin] ->
        procCommandArgs ("round-robin" :: defaultDeckPaths)
     | RoundRobin :: deckPaths ->
        roundRobinCommand deckPaths

    | ["effs"] ->
        showEffectsCommand ()

    | _ ->
        printfn "%s" (usage ()) |> pass

  [<EntryPoint>]
  let main argv =
    procCommandArgs (argv |> Array.toList)
    |> Trial.runConsoleApp
