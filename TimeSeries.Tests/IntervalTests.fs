module TimeSeries.IntervalTests

open Xunit
open Interval

let ( <|> ) a b = orElse a b

let interpret i x =
    (Interval.isUnbounded i, Interval.isEmpty i, Interval.includes i x)

let n = 10

let intervals =
    [
        yield Interval.empty
        yield Interval.unbounded
        for i in 1 .. n do
            for j in i .. n do
                yield Interval.bounded i j
    ]

let elements =
    [1..n]

let ( =?= ) a b =
    Seq.forall (fun x -> interpret a x = interpret b x) elements

let ( =??= ) a b =
    Seq.forall (fun x -> a x = b x) elements

(* Inclusion *)

[<Fact>]
let IncludesOk () =
    for i in 1 .. n do
        includes empty i =? false
        includes unbounded i =? true
        for j in i .. n do
            for k in 1 .. n do
                includes (bounded i j) k =? ((k >= i) && (k <= j))

(* Intersection *)

let intersectOk a b =
    includes (intersect a b) =??= fun x -> includes a x && includes b x

[<Fact>]
let IntersectOk () =
    check2 "intersect" intersectOk intervals intervals

(* Alternative laws *)

let orElseAssociative a b c =
    ((a <|> b) <|> c) =?= (a <|> (b <|> c))

let emptyLeftUnit x =
    empty <|> x =?= x

let emptyRightUnit x =
    x <|> empty =?= x

[<Fact>]
let OrElseAssociative () =
    check3 "orElseAssociative" orElseAssociative intervals intervals intervals

[<Fact>]
let EmptyRightUnit () =
    check "emptyRightUnit" emptyRightUnit intervals

[<Fact>]
let OrElseHasEmptyAsLeftUnit () =
    check "emptyLeftUnit" emptyLeftUnit intervals
