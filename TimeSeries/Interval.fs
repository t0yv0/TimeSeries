﻿[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TimeSeries.Interval

type Interval<'T when 'T : comparison> =
    | Bounded of 'T * 'T
    | Empty
    | Unbounded

let empty<'T when 'T : comparison> : Interval<'T> = Empty

let unbounded<'T when 'T : comparison> : Interval<'T> = Unbounded

let bounded lower upper =
    if upper < lower then
        invalidArg "upper" "upper bound should exceed the lower bound"
    Bounded (lower, upper)

let rec intersect a b =
    match a, b with
    | Empty, _ | _, Empty -> Empty
    | Unbounded, x | x, Unbounded -> x
    | Bounded (x, _), Bounded (y, _) when x > y -> intersect b a
    | Bounded (_, x), Bounded (y, z) when y <= x -> Bounded (y, min x z)
    | _ -> Empty

let includes iv x =
    match iv with
    | Bounded (a, b) -> x >= a && x <= b
    | Empty -> false
    | Unbounded -> true

let bounds iv =
    match iv with
    | Bounded (x, y) -> Some (x, y)
    | _ -> None

let lowerBound iv =
    match iv with
    | Bounded (x, _) -> Some x
    | _ -> None

let upperBound iv =
    match iv with
    | Bounded (_, x) -> Some x
    | _ -> None

let isUnbounded iv =
    match iv with
    | Unbounded _ -> true
    | _ -> false
