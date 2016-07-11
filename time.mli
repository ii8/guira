
type t

type interval
  = Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  | Months
  | Years
  | Eternity

module Month : sig
  type t
    = Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec
  val of_int : int -> t
  val to_int : t -> int
  val next : t -> t
end

module Day_of_week : sig
  type t
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun
  val of_int : int -> t
  val to_int : t -> int
end

val year : t -> int
val month : t -> Month.t
val day : t -> int
val hour : t -> int
val minute : t -> int
val second : t -> int

val next : t -> interval -> t
val diff : t -> t -> int

val tests : (string * (unit -> bool)) list
