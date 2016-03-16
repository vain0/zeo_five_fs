namespace ZeoFive.Cui

open ZeoFive.Core

module Error =
  let toString =
    function
    | CantOpenDeck path ->
        sprintf "Can't open deck: %s" path
    | CoreError err ->
        err |> Error.toString
