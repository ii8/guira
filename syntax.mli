open Sexp

exception Syntax of string

type interval = Days | Weeks | Months | Years | Eternity

type exp =
  | Variable
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

type bexp =
  | Equal_to_n of exp
  | Equal_to of exp * exp

type dayopt =
  | Weekday of Core.Std.Day_of_week.t

type monthopt =
  | Mensis of Core.Std.Month.t

type yearopt =
  | Annus of int

type 'a anyopt =
  | All
  | Not of 'a anyopt
  | Or of 'a anyopt list
  | And of 'a anyopt list
  | Nth of bexp * selector option
  | Opt of 'a

and selector =
  | Day of dayopt anyopt * selector list
  | Month of monthopt anyopt * selector list
  | Year of yearopt anyopt * selector list

val selector_of_sexp : sexp -> selector
