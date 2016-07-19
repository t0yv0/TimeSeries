/// Daily time-series with easy joins.
/// See http://www.haskellforall.com/2014/12/a-very-general-api-for-relational-joins.html
module TimeSeries.Daily

open System

/// Represents a dense per-day time-series carrying values of a given type.
type Daily<'T>

(*
    Semantics are determined by `lookup` and `interval` functions.
*)

/// Looks up the value on a given day. DateTime is assumed to reprsent a Date.
val lookup : Daily<'T> -> DateTime -> option<'T>

/// Interval of days on which the series is defined. Note that the interval can be All.
val interval : Daily<'T> -> Interval<DateTime>

/// Functor instance,
val map : ('A -> 'B) -> Daily<'A> -> Daily<'B>

(*
    Applicative: `always` and `apply` form an Applicative instance
    See https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html
*)

/// Defines a series defined on All keys with a given constant value.
val always : 'T -> Daily<'T>

/// Joins two series.
val apply : Daily<'A->'B> -> Daily<'A> -> Daily<'B>

(*
    Alternative: `empty` and `orElse` form an Alternative instance
    See https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html
*)

/// Defines a series that is defined on no keys.
val empty<'T> : Daily<'T>

/// The value of `orElse a b` is the value of `a` when defined, otherwise the value of `b`.
val orElse : Daily<'T> -> Daily<'T> -> Daily<'T>

(* Constructors *)

/// Constructs daily series starting on a given date, using the values given.
val range : DateTime -> seq<'T> -> Daily<'T>

/// Deduplicates, sorts and packs a sequence of tuples into a Daily series.
val ofSeq : seq<DateTime * 'T> -> Daily<'T>

/// Similar to ofSeq, but forgoes sorting and deduplication.
val ofSortedSeq : seq<DateTime * 'T> -> Daily<'T>

/// Sorts the samples and constructs a dense series by linear interpolation.
val linear : seq<DateTime * float> -> Daily<float>

(* Utility accessors *)

/// Reverse of `ofSeq`.
val toSeq : Daily<'T> -> seq<DateTime * 'T>

/// Dates of the series.
val dates : Daily<'T> -> DateTime[]

/// Values of the series.
val values : Daily<'T> -> 'T[]

type Daily<'T> with
    member Item : DateTime -> 'T with get
    member GetSlice : option<DateTime> * option<DateTime> -> Daily<'T>

(* Utility combinators *)

val reify : Daily<'T> -> Daily<'T>

val map2 : ('A -> 'B -> 'C) -> Daily<'A> -> Daily<'B> -> Daily<'C>
val map3 : ('A -> 'B -> 'C -> 'D) -> Daily<'A> -> Daily<'B> -> Daily<'C> -> Daily<'D>
val map4 : ('A -> 'B -> 'C -> 'D -> 'E) -> Daily<'A> -> Daily<'B> -> Daily<'C> -> Daily<'D> -> Daily<'E>

val optional : Daily<'T> -> Daily<option<'T>>
val withDefault : 'T -> Daily<'T> -> Daily<'T>

(* Interpolation *)

/// Stretches the series end to a given day.
val carryForward : DateTime -> Daily<'T> -> Daily<'T>

/// Stretches the series head back to a given day.
val carryBackward : DateTime -> Daily<'T> -> Daily<'T>
