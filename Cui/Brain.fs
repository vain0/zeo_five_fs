module Brain

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

