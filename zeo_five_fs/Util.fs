[<AutoOpen>]
module Util

[<RequireQualifiedAccess>]
module List =
  let tryMaxBy proj self =
    let folder st y =
      let projY = proj y
      in
        match st with
        | None -> Some (y, projY)
        | Some (x, projX) ->
            if projX < projY
            then Some (y, projY)
            else st
    in
      self |> List.fold folder None |> Option.map fst
