[<AutoOpen>]
module Util

[<AutoOpen>]
module Misc =
  let flip f x y = f y x

  type T5<'T> =
    'T * 'T * 'T * 'T * 'T

module T5 =
  let zip (x0, x1, x2, x3, x4) (y0, y1, y2, y3, y4) =
    ((x0, y0), (x1, y1), (x2, y2), (x3, y3), (x4, y4))

  let map f (x0, x1, x2, x3, x4) =
    (f x0, f x1, f x2, f x3, f x4)

  let replicate x =
    (x, x, x, x, x)

  let toList (x0, x1, x2, x3, x4) =
    [x0; x1; x2; x3; x4]

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
