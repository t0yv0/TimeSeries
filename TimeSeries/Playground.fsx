#r "bin/debug/TimeSeries.dll"

open System
open TimeSeries

let d1 = Daily.range DateTime.Today [1..1000]
let d2 = Daily.range (DateTime.Today.AddYears(1)) [1..1000]

let toA (x: Daily.Daily<'T>) =
    Seq.zip (Daily.dates x) (Daily.values x) |> Seq.toArray

let a1 = toA d1
let a2 = toA d2

let ( <*> ) f x = Daily.apply f x

#time

let test1 (n: int) =
    for i in 1 .. n do
        let d3 = Daily.select (+) <*> d1 |> Daily.leftJoin d2 -1
        let a3 = (Daily.values d3).[0]
        ()

let test2 (n: int) =
    for i in 1 .. n do
        query {
            for (d1, v1) in a1 do
              leftOuterJoin (d2, v2) in a2
                on (d1 = d2) into result
              for x in result do
                select (
                  if box x = null then
                    (v1 - 1)
                  else
                    (v1 + snd x)
                )
        }
        |> Seq.toArray
        |> fun x -> x.[0]
        |> ignore

test1 10000
test2 10000

