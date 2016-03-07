module Program

open System
open ZeoFive.Core

type ConsoleBroadcaster() =
  interface IListener with
    member this.Listen(g, ev) =
      match ev with
      | EvSummon cardId ->
          let card = g |> Game.card cardId
          let pl   = g |> Game.player (card |> Card.owner)
          do
            printfn "Player %s summoned %s."
              (pl.Name) (card.Spec.Name)

      | EvAttack (pl, way) ->
          let card = g |> Game.tryDohyoCard pl |> Option.get
          do
            printfn "%s attacked with %A."
              (card.Spec.Name) way

      | EvDamage (cardId, amount) ->
          let card      = g |> Game.card cardId
          let curHp     = card |> Card.curHp
          do
            printfn "%s took %d damage. (HP %d -> %d)"
              (card.Spec.Name) amount curHp (curHp - amount)

      | EvDie cardId ->
          let card = g.Board |> Map.find cardId
          do
            printfn "%s died." (card.Spec.Name)

      | EvGameBegin ->
          do
            printfn "-- %s vs %s --"
              (g |> Game.player Player1).Name
              (g |> Game.player Player2).Name

      | EvGameEnd (Win plId) ->
          let pl = g |> Game.player plId
          do
            printfn "Player %s wins!!" (pl.Name)

      | EvGameEnd (Draw) ->
          printfn "Draw."

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
  let audience =
    [
      ConsoleBroadcaster() :> IListener
    ]

  (pl1, pl2)
  ||> ZeoFive.Game.play audience
  |> ignore

  // exit code
  0
