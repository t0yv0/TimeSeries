module TimeSeries.Daily

open System

type Daily<'T> =
    {
        at : DateTime -> 'T
        interval : Interval<DateTime>
    }

let make at interval =
    {
        at = at
        interval = interval
    }

let select value =
    make (fun _ -> value) Interval.unbounded

let daysSince (start: DateTime) (d: DateTime) =
    (d - start).Days
 
let addDays (d: DateTime) (n: int) =
    d.AddDays(float n).Date

let range (start: DateTime) ts =
    let ts = Seq.toArray ts
    let n = Array.length ts
    let iv =
        match n with
        | 0 -> Interval.empty
        | _ -> Interval.bounded start.Date (addDays start (n - 1))
    make (daysSince start >> Array.get ts) iv

let map f daily =
    make (daily.at >> f) daily.interval

let apply f x =
    Interval.intersect f.interval x.interval
    |> make (fun d -> f.at d (x.at d))

let dates daily =
    match Interval.bounds daily.interval with
    | Some (a, b) -> Array.init ((b - a).Days + 1) (addDays a)
    | _ -> Array.empty

let values daily =
    dates daily |> Array.map daily.at

let interval daily =
    daily.interval

let at daily d =
    daily.at d

let leftJoin right missing left =
    let at d = left.at d (if Interval.includes right.interval d then right.at d else missing)
    make at left.interval
