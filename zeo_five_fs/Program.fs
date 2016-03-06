open ZeoFive.Core

[<EntryPoint>]
let main argv = 
  let replicate5 x = (x, x, x, x, x)

  let makeCard hp atk int' spd =
    {
      Name    = sprintf "{%d: %d/%d (%d)}" hp atk int' spd
      Hp      = hp
      Atk     = atk
      Int     = int'
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

  let result =
    ZeoFive.Game.play pl1 pl2

  printfn "%A" result

  // exit code
  0
