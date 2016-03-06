open ZeoFive.Core

[<EntryPoint>]
let main argv = 
  let replicate5 x = (x, x, x, x, x)

  let makeCard hp atk itl spd =
    {
      Name    = sprintf "{%d: %d/%d (%d)}" hp atk itl spd
      Hp      = hp
      Atk     = atk
      Itl     = itl
      Spd     = spd
    }

  let makeDeckByReplicate (card: CardSpec) =
    {
      Name    = sprintf "5x%s" (card.Name)
      Cards   = replicate5 card
    }

  let makeStupidPlayer name card =
    {
      Name    = name
      Deck    = makeDeckByReplicate card
      Brain   = (Brain.StupidBrain() :> IBrain)
    }

  let card1 = makeCard  50 50 50 50
  let card2 = makeCard 100 60 40  0
  let pl1   = makeStupidPlayer "50-person" card1
  let pl2   = makeStupidPlayer "164" card2

  match ZeoFive.Game.play pl1 pl2 with
  | (g, Win plId) ->
      let pl = g |> Game.player plId
      in printfn "Player %s wins." (pl.Name)
  | (_, Draw) ->
      printfn "Draw."

  // exit code
  0
