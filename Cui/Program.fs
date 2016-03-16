module ZeoFive.Cui.Program

open System.IO
open Chessie.ErrorHandling
open ZeoFive.Core

let tryLoadEntrantFromFile name path =
  trial {
    let! json =
      try
        File.ReadAllText(path) |> ok
      with
      | e -> CantOpenDeck path |> fail
    let! deck =
      Deck.ofJson(json)
      |> Deck.validate
      |> Trial.mapFailure (List.map CoreError)
    return
      {
        Name    = name
        Deck    = deck
        Brain   = Brain.StupidBrain()
      }
  }

[<EntryPoint>]
let main argv =
  let r =
    trial {
      let! ent1 = tryLoadEntrantFromFile "You" "you_deck.json"
      let! ent2 = tryLoadEntrantFromFile "CPU" "cpu_deck.json"
      let audience =
        [
          Broadcaster.broadcaster
        ]
      let r =
        (ent1, ent2)
        ||> Game.play audience
      return r
    }
  let () =
    match r with
    | Ok (_, _) -> ()
    | Bad (err) ->
        err |> List.iter (ZeoFive.Cui.Error.toString >> printfn "%s")

  // exit code
  0
