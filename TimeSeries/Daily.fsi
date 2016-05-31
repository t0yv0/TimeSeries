module TimeSeries.Daily

open System

type Daily<'T>

val select : 'T -> Daily<'T>

val range : DateTime -> seq<'T> -> Daily<'T>

val map : ('A -> 'B) -> Daily<'A> -> Daily<'B>

val apply : Daily<'A->'B> -> Daily<'A> -> Daily<'B>

val dates : Daily<'T> -> DateTime[]

val values : Daily<'T> -> 'T[]

val interval : Daily<'T> -> Interval<DateTime>

val at : Daily<'T> -> DateTime -> 'T

val leftJoin : right: Daily<'A> -> missing: 'A -> left: Daily<'A->'B> -> Daily<'B>

