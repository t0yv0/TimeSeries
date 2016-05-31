module TimeSeries.DailyTests

open System
open Daily

let days = Seq.init 20 (fun i -> DateTime.Today.AddDays(float i))
let ints = [1; 2]
let intArrays = [[||]; [| 1 |]; [| 1; 2 |]]

let dailys =
    seq {
        for i in ints do yield select i
        for d in Seq.take 15 days do for ar in intArrays do yield range d ar
    }

let coord iv (day: DateTime) =
    if Interval.isUnbounded iv then
        Some 0
    elif Interval.includes iv day then
        let delta = day - Option.get (Interval.lowerBound iv)
        Some delta.Days
    else
        None

let interpret d day =
    if Interval.includes (Daily.interval d) day then
        Some (Daily.at d day)
    else
        None

let optApply f x =
    match f, x with
    | Some f, Some x -> Some (f x)
    | _ -> None

let funcDailys =
    Seq.map (map (+)) dailys

[<Fact>]
let SelectOk () =
    let selectOk (v, d) =
        interpret (select v) d = Some v
    check "select" selectOk (x2 ints days)

[<Fact>]
let RangeOk () =
    let rangeOk (start, ts, d: DateTime) =
        let rd = range start ts
        interpret rd d = Option.map (Array.get ts) (coord (Daily.interval rd) d)
    check "range" rangeOk (x3 days intArrays days)

[<Fact>]
let MapOk () =
    let mapOk (daily, d) =
        let f = (+) 1
        interpret (map f daily) d = Option.map f (interpret daily d)
    check "map" mapOk (x2 dailys days)

[<Fact>]
let ApplyOk () =
    let applyOk (df, dx, d) =
        interpret (apply df dx) d = optApply (interpret df d) (interpret dx d)
    check "apply" applyOk (x3 funcDailys dailys days)

[<Fact>]
let ValuesOk () =
    let valuesOk daily =
        if Interval.isUnbounded (Daily.interval daily) then
            values daily = Array.empty
        else
            days
            |> Seq.filter (Interval.includes (Daily.interval daily))
            |> Seq.map (Daily.at daily)
            |> Seq.toArray = values daily
    check "values" valuesOk dailys

[<Fact>]
let LeftJoinOk () =
    let leftJoinOk (left, right, d) =
        let na = -1
        let example = left |> Daily.leftJoin right na
        let expected =
            match interpret left d, interpret right d with
            | Some l, Some r -> Some (l r)
            | Some l, None -> Some (l na)
            | _ -> None
        interpret example d = expected
    check "leftJoin" leftJoinOk (x3 funcDailys dailys days)
