module TimeSeries.IntervalTests

open Xunit
open Interval

let n = 10

let intervals =
    [
        yield Interval.empty
        yield Interval.unbounded
        for i in 1 .. n do
            for j in i .. n do
                yield Interval.bounded i j
    ]

[<Fact>]
let IncludesOk () =
    for i in 1 .. n do
        Interval.includes Interval.empty i =? false
        Interval.includes Interval.unbounded i =? true
        for j in i .. n do
            for k in 1 .. n do
                Interval.includes (Interval.bounded i j) k =? ((k >= i) && (k <= j))

let intersectOk (a, b, x) =
    includes (intersect a b) x = (includes a x && includes b x)

[<Fact>]
let IntersectOk () =
    x3 intervals intervals [1..n]
    |> check "intersect" intersectOk
