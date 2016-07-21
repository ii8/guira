open Sexp

exception Syntax of string

type exp =
  | Variable
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

type bexp =
  | Equal_to_n of exp
  | Equal_to of exp * exp
  | Greater_than of exp * exp

type minopt = Minuta of int

type houropt =
  | Hora of int

type dayopt =
  | Weekday of Time.Day_of_week.t

type monthopt =
  | Mensis of Time.Month.t

type yearopt =
  | Annus of int
  | Leap

type 'a anyopt =
  | All
  | Not of 'a anyopt
  | Or of 'a anyopt list
  | And of 'a anyopt list
  | Nth of bexp * 'a anyopt option
  | Opt of 'a

and selector =
  | Minute of minopt anyopt * selector list
  | Hour of houropt anyopt * selector list
  | Day of dayopt anyopt * selector list
  | Week of unit anyopt * selector list
  | Month of monthopt anyopt * selector list
  | Year of yearopt anyopt * selector list

val selector_of_sexp : sexp -> selector
