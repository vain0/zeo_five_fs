namespace ZeoFive.Cui

open ZeoFive.Core

type CuiError =
  | CoreError       of Error
  | CantOpenDeck    of string
