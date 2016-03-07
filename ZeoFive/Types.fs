namespace ZeoFive.Core

[<AutoOpen>]
module Types =
  type PlayerId =
    | Player1
    | Player2

  type NPCardId =
    | Card1
    | Card2
    | Card3
    | Card4
    | Card5

  type CardId =
    PlayerId * NPCardId

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
    | PhGameBegin
    | PhGameEnd       of GameResult
    | PhSummon        of PlayerId
    | PhCombat        of Set<PlayerId>

  type Event =
    | EvSummonSelect  of PlayerId
    | EvSummon        of CardId
    | EvCombat        of Set<PlayerId>
    | EvAttackSelect  of PlayerId
    | EvAttack        of PlayerId * AttackWay
    | EvDamage        of CardId * int
    | EvDie           of CardId
    | EvGameBegin
    | EvGameEnd       of GameResult

  type GameState =
    {
      Board         : Board
      Dohyo         : Dohyo
    }

  type IBrain =
    abstract member Summon: PlayerId * GameState -> CardId
    abstract member Attack: PlayerId * GameState -> AttackWay

  type Player =
    {
      Name    : string
      Deck    : Deck
      Brain   : IBrain
    }

  type GameStateFromListener =
    {
      Players     : Player * Player
      Board       : Board
      Dohyo       : Dohyo
    }

  type Game =
    {
      Players     : Player * Player
      Board       : Board
      Dohyo       : Dohyo
      Kont        : Event list
      Audience    : IListener list
    }

  and IListener =
    abstract member Listen: Game * Game * Event -> unit
