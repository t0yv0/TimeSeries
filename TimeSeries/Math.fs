module TimeSeries.Math

open System

let interpolateLinear (x1: float) y1 x2 y2 =
    let b = (y2 - y1) / (x2 - x1)
    fun x -> y1 + b * x

let interpolateLinearOnDomain (toF: 'T -> float) (x1: 'T) (y1: float) (x2: 'T) (y2: float) =
    toF >> interpolateLinear (toF x1) y1 (toF x2) y2

let interpolateLinearOnDateTimeDomain (x1: DateTime) y1 x2 y2 =
    let toF (x: DateTime) = (x - x1).TotalDays
    interpolateLinearOnDomain toF x1 y1 x2 y2
