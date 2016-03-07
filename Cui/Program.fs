module Program

open ZeoFive.Core
open Brain
open Broadcaster

[<AutoOpen>]
module Helper =
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
      Cards   = T5.replicate card
    }

  let makeStupidPlayer name card =
    {
      Name    = name
      Deck    = makeDeckByReplicate card
      Brain   = (Brain.StupidBrain() :> IBrain)
    }

[<EntryPoint>]
let main argv = 

  let card1 = makeCard  50 50 50 50
  let card2 = makeCard 100 60 40  0
  let pl1   = makeStupidPlayer "50-person" card1
  let pl2   = makeStupidPlayer "164" card2
  let audience =
    [
      ConsoleBroadcaster() :> IListener
    ]

  (pl1, pl2)
  ||> ZeoFive.Game.play audience
  |> ignore

  // exit code
  0
