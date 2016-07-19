[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TimeSeries.Interval

type Interval<'T when 'T : comparison>

val empty<'T when 'T : comparison> : Interval<'T>

val unbounded<'T when 'T : comparison> : Interval<'T>

val bounded : lowerBound: 'T -> upperBound: 'T -> Interval<'T>

val intersect : Interval<'T> -> Interval<'T> -> Interval<'T>

val includes : Interval<'T> -> 'T -> bool

val bounds : Interval<'T> -> option<'T * 'T>

val lowerBound : Interval<'T> -> option<'T>

val upperBound : Interval<'T> -> option<'T>

val isUnbounded : Interval<'T> -> bool

val isEmpty : Interval<'T> -> bool
val isBounded : Interval<'T> -> bool
val orElse : Interval<'T> -> Interval<'T> -> Interval<'T>

val extendForward : 'T -> Interval<'T> -> Interval<'T>
val extendBackward : 'T -> Interval<'T> -> Interval<'T>
