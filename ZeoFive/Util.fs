[<AutoOpen>]
module Util

open System

[<AutoOpen>]
module Misc =
  let flip f x y = f y x
  let tap f x = f x; x

  type T5<'T> =
    'T * 'T * 'T * 'T * 'T

[<RequireQualifiedAccess>]
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

type Cont<'r,'t> =
  | Cont of (('t -> 'r) -> 'r)

[<RequireQualifiedAccess>]
module Cont =
  let run (Cont x) = (x: ('T -> 'R) -> 'R)

  let callCC (f: ('T -> Cont<'R, 'U>) -> _) =
    Cont (fun k -> run (f (fun a -> Cont (fun _ -> k a))) k)

  let result x = Cont (fun k -> k x)

  let map  (f: 'T -> _) (Cont x) : Cont<_, _> = Cont (fun c -> x (c << f))
  let bind (f: 'T -> _) (Cont x) : Cont<_, _> = Cont (fun k -> x (fun a -> run (f a) k))          
  let apply (Cont f) (Cont x)    : Cont<_, _> = Cont (fun k -> f (fun (f': 'T -> _) -> x (k << f')))

  type ContBuilder () =
    member this.Run(f) = f ()

    member this.Delay(f) = f

    member this.Return(x) =
      result x

    member this.ReturnFrom(expr) = expr

    member this.Bind(p, rest) =
      bind rest p

    member this.Using(x: #IDisposable, f) =
      try f x
      finally
        match box x with
        | null -> ()
        | notNull -> x.Dispose()

    member this.Combine(x, rest) =
      rest ()

    member this.TryWith(f, h) =
      try f ()
      with | e -> h e

    member this.TryFinally(f, g) =
      try f ()
      finally g ()

[<AutoOpen>]
module ContMonadSyntax =
  let cont = Cont.ContBuilder()

type StateT<'s, 'm> =
  | StateT of ('s -> 'm)

[<RequireQualifiedAccess>]
module StateT =
  let run (StateT x) = x

[<RequireQualifiedAccess>]
module StateCont =
  let result x =
    StateT (fun s -> Cont.result (x, s))

  let bind f (StateT m) =
    StateT (fun s -> Cont.bind (fun (a, s') -> StateT.run (f a) s') (m s))

  let liftCont m =
    StateT (fun s -> Cont.bind (fun a -> Cont.result (a, s)) m)

  let get   = StateT (fun s -> Cont.result (s, s))
  let put s = StateT (fun _ -> Cont.result ((), s))

  let callCC f =
    StateT (fun s -> Cont.callCC (fun cc -> StateT.run (f cc) s))

  type StateContBuilder () =
    member this.Run(f) = f ()
    member this.Delay(f) = f
    member this.Zero() = result ()
    member this.Return(x) = result x
    member this.ReturnFrom(m) = m
    member this.Bind(x, f) = bind f x
    member this.Combine(x, k) = x; k ()

[<AutoOpen>]
module StateContSyntax =
  let stcont = StateCont.StateContBuilder()

[<RequireQualifiedAccess>]
module Observable =
  open System.Diagnostics

  let indexed obs =
    obs
    |> Observable.scan
        (fun (opt, i) x -> (Some x, i + 1)) (None, -1)
    |> Observable.choose
        (fun (opt, i) -> opt |> Option.map (fun x -> (x, i)))

  type Source<'T>() =
    let protect function1 =
      let mutable ok = false
      try 
        function1()
        ok <- true
      finally
        Debug.Assert(ok, "IObserver method threw an exception.")

    let mutable key = 0
    let mutable subscriptions = (Map.empty: Map<int, IObserver<'T>>)

    let thisLock = new obj()

    let subscribe obs =
      let body () =
        key |> tap (fun k ->
          do key <- k + 1
          do subscriptions <- subscriptions |> Map.add k obs
          )
      in lock thisLock body

    let unsubscribe k =
      let body () =
        subscriptions <- subscriptions |> Map.remove k
      in
        lock thisLock body

    let next obs =
      subscriptions |> Map.iter (fun _ value ->
        protect (fun () -> value.OnNext(obs)))

    let completed () =
      subscriptions |> Map.iter (fun _ value ->
        protect (fun () -> value.OnCompleted()))

    let error err =
      subscriptions |> Map.iter (fun _ value ->
        protect (fun () -> value.OnError(err)))

    let obs = 
      { new IObservable<'T> with
          member this.Subscribe(obs) =
            let cancelKey = subscribe obs
            { new IDisposable with 
                member this.Dispose() = unsubscribe cancelKey
                }
          }

    let mutable finished = false

    member this.Next(obs) =
      Debug.Assert(not finished, "IObserver is already finished")
      next obs

    member this.Completed() =
      Debug.Assert(not finished, "IObserver is already finished")
      finished <- true
      completed()

    member this.Error(err) =
      Debug.Assert(not finished, "IObserver is already finished")
      finished <- true
      error err

    member this.AsObservable = obs
