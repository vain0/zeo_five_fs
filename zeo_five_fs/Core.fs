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
  let init side spec =
    {
      Spec    = spec
      Damage  = 0
      Side    = side
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

module PlayerSide =
  let inverse =
    function
    | First  -> Second
    | Second -> First

module AttackWay =
  let inverse =
    function
    | PhysicalAttack -> MagicalAttack
    | MagicalAttack  -> PhysicalAttack

module Game =
  let init pl1 pl2 =
    let deckInit side (pl: Player) =
      pl.Deck
      |> Deck.cardList
      |> List.map (fun card -> (card, side))
    let initBoard =
      List.append
        (pl1 |> deckInit First )
        (pl2 |> deckInit Second)
      |> List.map (fun (spec, side) ->
          let c = Card.init side spec
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

  let playerOn side (g: Game) =
    let f =
      match side with
      | First  -> fst
      | Second -> snd
    in
      g.Players |> f

  /// side 側からみた場況
  let state side (g: Game): GameState =
    {
      Board =
        g.Board
        |> Map.filter (fun _ card -> card.Side = side)
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
