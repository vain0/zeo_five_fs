namespace ZeoFive.Core

[<AutoOpen>]
module Misc =
  let newCardId =
    let r = ref 0
    fun () ->
      let i = ! r
      do r := i + 1
      i |> CardId

  let flip f x y = f y x

module Card =
  let init pl spec =
    {
      Spec    = spec
      Damage  = 0
      Owner   = pl
      CardId  = newCardId ()
    }

  let curHp (card: Card) =
    card.Spec.Hp - card.Damage

  let isAlive card =
    curHp card > 0

  let isDead card =
    card |> isAlive |> not

  let power attackWay (card: Card) =
    match attackWay with
    | PhysicalAttack -> card.Spec.Atk
    | MagicalAttack  -> card.Spec.Itl

module Deck =
  let cardList (deck: Deck) =
    let (c0, c1, c2, c3, c4) = deck.Cards
    in [c0; c1; c2; c3; c4]

module Player =
  let inverse =
    function
    | Player1 -> Player2
    | Player2 -> Player1

module AttackWay =
  let inverse =
    function
    | PhysicalAttack -> MagicalAttack
    | MagicalAttack  -> PhysicalAttack

module Game =
  let init pl1 pl2 =
    let deckInit plId (pl: Player) =
      pl.Deck
      |> Deck.cardList
      |> List.map (fun card -> (card, plId))
    let initBoard =
      List.append
        (pl1 |> deckInit Player1)
        (pl2 |> deckInit Player2)
      |> List.map (fun (spec, plId) ->
          let c = Card.init plId spec
          in (c.CardId, c)
          )
      |> Map.ofList
    in
      {
        Players       = (pl1, pl2)
        Board         = initBoard
        Battlefield   = Map.empty
        Phase         = GameBegin
      }

  let player pl (g: Game) =
    let f =
      match pl with
      | Player1 -> fst
      | Player2 -> snd
    in
      g.Players |> f

  let card cardId (g: Game) =
    g.Board |> Map.find cardId

  /// プレイヤー pl からみた場況
  let state pl (g: Game): GameState =
    {
      Board =
        g.Board
        |> Map.filter (fun _ card -> card.Owner = pl)
      Battlefield =
        g.Battlefield
    }

module Brain =
  type StupidBrain() =
    interface IBrain with
      member this.Summon(state: GameState) =
        state.Board
        |> Map.filter (fun _ card -> card |> Card.isAlive)
        |> Map.toList
        |> List.head  // assert that someone is alive
        |> fst
      member this.Attack(state: GameState) =
        PhysicalAttack
