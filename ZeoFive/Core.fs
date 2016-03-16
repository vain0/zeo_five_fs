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

module Deck =
  let toJson self =
    Serialize.serializeJson<Deck>(self)

  let ofJson json =
    Serialize.deserializeJson<Deck>(json)

module Player =
  let allIds =
    [Player1; Player2]

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

module Brain =
  type StupidBrain() =
    interface IBrain with
      member this.Summon(state: GameStateFromPlayer) =
        state.Player.Hand
        |> List.head

      member this.Attack(state: GameStateFromPlayer) =
        PhysicalAttack
