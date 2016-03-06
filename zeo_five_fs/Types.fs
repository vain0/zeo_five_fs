namespace ZeoFive.Core

[<AutoOpen>]
module Types =
  type CardId =
    | CardId of int

  type internal T5<'T> =
    'T * 'T * 'T * 'T * 'T
    
  type PlayerId =
    | Player1
    | Player2

  type AttackWay =
    | PhysicalAttack
    | MagicalAttack

  type CardSpec =
    {
      Name    : string
      Hp      : int
      Atk     : int
      Itl     : int  // intelligence
      Spd     : int
    }

  type Card =
    {
      Spec    : CardSpec
      CardId  : CardId
      Owner   : PlayerId
      Damage  : int
      PrevWay : option<AttackWay>
    }

  type Deck =
    {
      Name    : string
      Cards   : T5<CardSpec>
    }

  type Board =
    Map<CardId, Card>

  type Dohyo =
    Map<PlayerId, CardId>

  type GameResult =
    | Win           of PlayerId
    | Draw

  type Phase =
    | GameBegin
    | GameEnd       of GameResult
    | SummonPhase   of PlayerId
    | CombatPhase
    | AttackPhase   of PlayerId list

  type GameState =
    {
      Board         : Board
      Dohyo         : Dohyo
    }

  type IBrain =
    abstract member Summon: GameState -> CardId
    abstract member Attack: GameState -> AttackWay

  type Player =
    {
      Name    : string
      Deck    : Deck
      Brain   : IBrain
    }

  type Game =
    {
      Players     : Player * Player
      Board       : Board
      Dohyo       : Dohyo
      Phase       : Phase
    }
