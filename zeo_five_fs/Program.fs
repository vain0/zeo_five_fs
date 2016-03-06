module Program

open System
open ZeoFive.Core

type ConsoleBrain() =
  interface IBrain with
    member this.Summon(pl, state) =
      let npcardIdFromInt =
        NPCardId.indexed |> Map.ofList
      let rec loop () =
        printfn "召喚するカードを選んでください: "
        match Console.ReadLine() |> Int32.TryParse with
        | (true, i)
          when npcardIdFromInt |> Map.containsKey i ->
            let cardId = (pl, npcardIdFromInt |> Map.find i)
            let card   = state.Board |> Map.find cardId
            if card |> Card.isDead
            then
              eprintfn "%s は既に死亡しています。" (card.Spec.Name)
              loop ()
            else cardId
        | _ ->
          eprintfn "0~4 の番号で選んでください。"
          loop ()
      in loop ()

    member this.Attack(pl, state) =
      printfn "Attack with...? (p/m)"
      let rec loop () =
        match Console.ReadLine() with
        | "p" | "P" -> PhysicalAttack
        | "m" | "M" -> MagicalAttack
        | _ ->
            eprintfn "Invalid input" 
            loop ()
      in loop ()

[<EntryPoint>]
let main argv = 
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
