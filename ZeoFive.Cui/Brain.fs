namespace ZeoFive.Cui

open System
open ZeoFive.Core

type ConsoleBrain() =
  interface IBrain with
    member this.Summon(state) =
      let npcardIdFromInt =
        NPCardId.indexed |> Map.ofList
      let rec loop () =
        printfn "召喚するカードを選んでください: "
        match Console.ReadLine() |> Int32.TryParse with
        | (true, i)
          when npcardIdFromInt |> Map.containsKey i ->
            let cardId =
              ( state.Player.PlayerId
              , npcardIdFromInt |> Map.find i
              )
            let card = state.CardStore |> Map.find cardId
            in
              if card |> Card.isDead
              then
                eprintfn "%s は既に死亡しています。" (card.Spec.Name)
                loop ()
              else cardId
        | _ ->
          eprintfn "0~4 の番号で選んでください。"
          loop ()
      in loop ()

    member this.Attack(state) =
      printfn "Attack with...? (p/m)"
      let rec loop () =
        match Console.ReadLine() with
        | "p" | "P" -> PhysicalAttack
        | "m" | "M" -> MagicalAttack
        | _ ->
            eprintfn "Invalid input" 
            loop ()
      in loop ()

