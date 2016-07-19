[<AutoOpen>]
module TimeSeries.Util

open Xunit
type FactAttribute = Xunit.FactAttribute

let check name ok universe =
    let fail e = failwithf "%s failed on: %A" name e
    let checks = ref 0
    universe
    |> Seq.tryFind (fun example ->
        incr checks
        try not (ok example) with e -> eprintfn "%O" e; true)
    |> Option.iter fail
    printfn "%20s - %i checks" name !checks

let x2 a b =
    seq { for x in a do for y in b do yield (x, y) }

let x3 a b c =
    seq { for x in a do for y in b do for z in c do yield (x, y, z) }

let x4 a b c d =
    seq { for x in a do for y in b do for z in c do for zz in d do yield (x, y, z, zz) }

let uncurry2 f (a, b) = f a b
let uncurry3 f (a, b, c) = f a b c
let uncurry4 f (a, b, c, d) = f a b c d

let check2 name law xs ys =
    check name (uncurry2 law) (x2 xs ys)

let check3 name law xs ys zs =
    check name (uncurry3 law) (x3 xs ys zs)

let assertEqual<'T when 'T : equality> (actual: 'T) (expected: 'T) =
    Assert.Equal<'T>(expected, actual)

let inline ( =? ) actual expected =
    assertEqual actual expected
