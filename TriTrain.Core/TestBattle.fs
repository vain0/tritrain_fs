namespace TriTrain.Core

open System.Threading

module TestBattle =
  let runGameWithObserver observe (pl1, pl2): Game * GameResult =
    let g = Game.create pl1 pl2
    use o = observe g
    in g |> Game.run

  let calcGameResult (pl1, pl2): GameResult =
    let g = Game.create pl1 pl2
    in g |> Game.run |> snd

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
    let (_: unit []) =
      [ for _ in 1..times ->
          async {
            return calcGameResult plPair |> inc
          }
      ]
      |> Async.Parallel
      |> Async.RunSynchronously
    in (win, lose, draw)

  /// 総当たりでテストバトルを行う
  let roundRobin times pls =
    Seq.product pls pls
    |> Seq.map (fun plPair -> async {
        return (plPair, testBattle 10 plPair)
        })
    |> Async.Parallel
    |> Async.RunSynchronously
