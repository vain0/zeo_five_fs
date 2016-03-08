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

  let init plId (entrant: Entrant): Player =
    {
      PlayerId      = plId
      Name          = entrant.Name
      Brain         = entrant.Brain
      Hand =
        NPCardId.all
        |> T5.map (fun cId -> (plId, cId))
        |> T5.toList
      Dohyo         = None
    }

  let exterior (self: Player) =
    {
      PlayerId      = self.PlayerId
      Name          = self.Name
      HandCount     = self.Hand |> List.length
      Dohyo         = self.Dohyo
    }

module AttackWay =
  let inverse =
    function
    | PhysicalAttack -> MagicalAttack
    | MagicalAttack  -> PhysicalAttack

module Game =
  let init ent1 ent2 =
    let deckInit plId (ent: Entrant) =
      T5.zip
        (NPCardId.all |> T5.map (fun c -> (plId, c)))
        (ent.Deck.Cards)
      |> T5.toList
      |> List.map (fun (cardId, spec) ->
          let card = Card.init cardId spec
          in (cardId, card)
          )
    let initCardStore =
      List.append
        (ent1 |> deckInit Player1)
        (ent2 |> deckInit Player2)
      |> Map.ofList
    let initPlayerStore =
      [
        (Player1, Player.init Player1 ent1)
        (Player2, Player.init Player2 ent2)
      ]
      |> Map.ofList
    in
      {
        PlayerStore   = initPlayerStore
        CardStore     = initCardStore
        Kont          = [EvGameBegin]
      }

  let player pl (g: Game) =
    g.PlayerStore |> Map.find pl

  let card cardId (g: Game) =
    g.CardStore |> Map.find cardId

  let tryDohyoCard pl (g: Game) =
    (g |> player pl).Dohyo
    |> Option.map (fun cardId -> g |> card cardId)

  let dohyoCards (g: Game) =
    g.PlayerStore
    |> Map.toList
    |> List.choose (fun (_, player) -> player.Dohyo)
    |> Set.ofList

  /// プレイヤーにカードが見えているか？
  let isRevealedTo pl cardId (g: Game) =
    [
      (g |> card cardId |> Card.owner = pl)
      (g |> dohyoCards |> Set.contains cardId)
    ] |> List.exists id

  /// プレイヤー pl からみた場況
  let state pl (g: Game): GameStateFromPlayer =
    {
      Player =
        g |> player pl
      Opponent =
        g |> player (pl |> Player.inverse) |> Player.exterior
      CardStore =
        g.CardStore
        |> Map.filter (fun _ card -> g |> isRevealedTo pl (card.CardId))
    }

  let happen ev (g: Game) =
    { g with
        Kont = ev :: g.Kont
      }

  let endWith r g =
    { g with Kont = [EvGameEnd r] }

  let updatePlayer pl player (g: Game) =
    { g with
        PlayerStore = g.PlayerStore |> Map.add pl player
      }

  let updateDohyo pl cardId (g: Game) =
    let player =
      { (g |> player pl) with Dohyo = Some cardId }
    in
      g |> updatePlayer pl player

  let updateHand pl f (g: Game) =
    let player = g |> player pl
    let player = { player with Hand = (player.Hand |> f) }
    in
      g |> updatePlayer pl player

  let updateCard cardId card (g: Game) =
    assert (card.CardId = cardId)
    { g with
        CardStore =
          g.CardStore |> Map.add cardId card
      }

module Brain =
  type StupidBrain() =
    interface IBrain with
      member this.Summon(state: GameStateFromPlayer) =
        state.CardStore
        |> Map.filter (fun _ card -> card |> Card.isAlive)
        |> Map.toList
        |> List.head  // assert that someone is alive
        |> fst
      member this.Attack(state: GameStateFromPlayer) =
        PhysicalAttack
