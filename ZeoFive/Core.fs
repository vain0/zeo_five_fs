namespace ZeoFive.Core

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

  let init (entrant: Entrant): Player =
    {
      Name    = entrant.Name
      Brain   = entrant.Brain
    }

module AttackWay =
  let inverse =
    function
    | PhysicalAttack -> MagicalAttack
    | MagicalAttack  -> PhysicalAttack

module Game =
  let init audience ent1 ent2 =
    let deckInit plId (ent: Entrant) =
      T5.zip
        (NPCardId.all |> T5.map (fun c -> (plId, c)))
        (ent.Deck.Cards)
      |> T5.toList
      |> List.map (fun (cardId, spec) ->
          let card = Card.init cardId spec
          in (cardId, card)
          )
    let initBoard =
      List.append
        (ent1 |> deckInit Player1)
        (ent2 |> deckInit Player2)
      |> Map.ofList
    let pl1 = Player.init ent1
    let pl2 = Player.init ent2
    in
      {
        Players       = (pl1, pl2)
        Board         = initBoard
        Dohyo         = Map.empty
        Kont          = [EvGameBegin]
        Audience      = audience
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

  let happen ev (g: Game) =
    { g with
        Kont = ev :: g.Kont
      }

  let endWith r g =
    { g with Kont = [EvGameEnd r] }

  let updateDohyo pl cardId (g: Game) =
    { g with
        Dohyo =
          g.Dohyo |> Map.add pl cardId
      }

  let updateCard cardId card (g: Game) =
    assert (card.CardId = cardId)
    { g with
        Board =
          g.Board |> Map.add cardId card
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
