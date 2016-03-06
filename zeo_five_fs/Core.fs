namespace ZeoFive.Core

[<AutoOpen>]
module Misc =
  let flip f x y = f y x

module T5 =
  let zip (x0, x1, x2, x3, x4) (y0, y1, y2, y3, y4) =
    ((x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4))

  let map f (x0, x1, x2, x3, x4) =
    (f x0, f x1, f x2, f x3, f x4)

  let replicate x =
    (x, x, x, x, x)

  let toList (x0, x1, x2, x3, x4) =
    [x0; x1; x2; x3; x4]

module NPCardId =
  let all = (Card1, Card2, Card3, Card4, Card5)

  let indexed =
    T5.zip (0, 1, 2, 3, 4) all |> T5.toList

module Card =
  let init cardId spec =
    {
      Spec    = spec
      CardId  = cardId
      Damage  = 0
      PrevWay = None
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

  let owner (card: Card) =
    card.CardId |> fst

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
      T5.zip
        (NPCardId.all |> T5.map (fun c -> (plId, c)))
        (pl.Deck.Cards)
      |> T5.toList
      |> List.map (fun (cardId, spec) ->
          let card = Card.init cardId spec
          in (cardId, card)
          )
    let initBoard =
      List.append
        (pl1 |> deckInit Player1)
        (pl2 |> deckInit Player2)
      |> Map.ofList
    in
      {
        Players       = (pl1, pl2)
        Board         = initBoard
        Dohyo         = Map.empty
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

  let tryDohyoCard pl (g: Game) =
    g.Dohyo
    |> Map.tryFind pl
    |> Option.map (flip card g)

  /// プレイヤー pl からみた場況
  let state pl (g: Game): GameState =
    {
      Board =
        g.Board
        |> Map.filter (fun _ card -> (card |> Card.owner) = pl)
      Dohyo =
        g.Dohyo
    }

module Brain =
  type StupidBrain() =
    interface IBrain with
      member this.Summon(_, state: GameState) =
        state.Board
        |> Map.filter (fun _ card -> card |> Card.isAlive)
        |> Map.toList
        |> List.head  // assert that someone is alive
        |> fst
      member this.Attack(_, state: GameState) =
        PhysicalAttack
