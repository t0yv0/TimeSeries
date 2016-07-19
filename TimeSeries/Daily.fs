module TimeSeries.Daily

open System
open System.IO

let addDays (d: DateTime) (n: int) =
    d.AddDays(float n).Date

let intervalDates (interval: Interval<DateTime>) =
    match Interval.bounds interval with
    | Some (a, b) -> Array.init ((b - a).Days + 1) (addDays a)
    | _ -> Array.empty

let showDate (d: DateTime) =
    d.ToString("yyyy-MM-dd")

type Daily<'T> =
    {
        at : DateTime -> 'T
        interval : Interval<DateTime>
    }

    override this.ToString() =
        if Interval.isUnbounded this.interval then
            "<fun>"
        elif Interval.isEmpty this.interval then
            "<emp>"
        else
            use w = new StringWriter()
            fprintfn w ""
            let (d1, d2) = (Interval.bounds this.interval).Value
            let dayCount = int (d2 - d1).TotalDays + 1
            fprintfn w "[%s .. %s] - %i days" (showDate d1) (showDate d2) dayCount
            fprintfn w "----------"
            for d in Seq.truncate 30 (intervalDates this.interval) do
                let v = this.at d
                fprintfn w "%s %O" (showDate d) v
            if dayCount > 30 then
                fprintfn w "..."
            w.ToString()

let make at interval =
    { at = at; interval = interval }

let at d dt = d.at dt
let interval d = d.interval

let lookup d dt =
    if Interval.includes (interval d) dt then Some (at d dt) else None

let map f daily =
    make (at daily >> f) (interval daily)

let always value =
    make (fun _ -> value) Interval.unbounded

let apply f x =
    Interval.intersect (interval f) (interval x)
    |> make (fun d -> (at f d) (at x d))

(* NOTE: make sure that `orElse` is associative and `empty` is its left and right unit. *)

let empty<'T> : Daily<'T> =
    make (fun _ -> failwith "empty") Interval.empty

let orElse a b =
    let at d =
        match lookup a d with
        | None -> at b d
        | Some v -> v
    make at (Interval.orElse (interval a) (interval b))

let daysSince (start: DateTime) (d: DateTime) =
    (d - start).Days
 
let range (start: DateTime) ts =
    if start <> start.Date then
        failwith "start date is not a date"
    let ts = Seq.toArray ts
    let n = Array.length ts
    let iv =
        match n with
        | 0 -> Interval.empty
        | _ -> Interval.bounded start.Date (addDays start (n - 1))
    make (daysSince start >> Array.get ts) iv

let dates daily =
    intervalDates (interval daily)

let values daily =
    dates daily |> Array.map (at daily)

type Daily<'T> with
    member this.Item with get (key) = at this key

    member this.GetSlice(a: option<DateTime>, b: option<DateTime>) =
        let i = interval this
        let slice a b =
            make (at this) (Interval.intersect (Interval.bounded a b) i)
        match a, b with
        | Some a, Some b -> slice a b
        | Some a, None ->
            match Interval.upperBound i with
            | Some b -> slice a b
            | _ -> failwith "Missing an upper bound"
        | None, Some b ->
            match Interval.lowerBound i with
            | Some a -> slice a b
            | _ -> failwith "Missing a lower bound"
        | None, None -> this

let toSeq daily =
    Seq.zip (dates daily) (values daily)

let ofSortedSeq pairs =
    let (dates, values) = pairs |> Seq.toArray |> Array.unzip
    if Array.isEmpty dates then empty else
        let isDaily =
            dates
            |> Seq.pairwise
            |> Seq.forall (fun (a: DateTime, b: DateTime) ->
                a = a.Date && b = b.Date && (b - a).TotalDays = 1.)
        if not isDaily then
            failwith "Input sequence is not daily"
        let d0 = dates.[0]
        range dates.[0] values

let ofSeq pairs =
    pairs
    |> Seq.distinctBy fst
    |> Seq.sortBy fst
    |> ofSortedSeq

let ( <*> ) f x = apply f x
let ( <|> ) a b = orElse a b
let map2 f a b = always f <*> a <*> b
let map3 f a b c = always f <*> a <*> b <*> c
let map4 f a b c d = always f <*> a <*> b <*> c <*> d
let optional d = map Some d <|> always None
let withDefault v d = d <|> always v

let reify d =
    let i = interval d
    match Interval.bounds i with
    | Some (a, b) -> range a (values d)
    | _ -> d

// Interpolation --------------------------------------------------------------

let linearDailySegment dx x dy y =
    Interval.bounded dx dy
    |> intervalDates
    |> Array.map (fun d -> (d, Math.interpolateLinearOnDateTimeDomain dx x dy y d))

let linear (pairs: seq<DateTime * float>) =
    let pairs =
        pairs
        |> Seq.map (fun (d, v) -> (d.Date, v))
        |> Seq.distinctBy fst
        |> Seq.sortBy fst
        |> Seq.toArray
    match pairs.Length with
    | 0 -> empty
    | 1 -> ofSortedSeq pairs
    | _ ->
        Seq.pairwise pairs
        |> Seq.mapi (fun i ((dx, x), (dy, y)) ->
            let segment = linearDailySegment dx x dy y
            if i = 0 then
                segment
            else
                segment.[1..])
        |> Seq.concat
        |> ofSortedSeq

let inline carry extend bound date d =
    let def = lazy at d (Option.get (bound (interval d)))
    let at x =
        match lookup d x with
        | None -> def.Value
        | Some v -> v
    let iv = extend date (interval d)
    make at iv

let carryForward date d =
    carry Interval.extendForward Interval.upperBound date d

let carryBackward date d =
    carry Interval.extendBackward Interval.lowerBound date d
