
exception Syntax of string

type exp =
  | Variable
  | Var_week_of_month
  | Var_day_of_month
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

type bexp =
  | All
  | Not of bexp
  | Or of bexp list
  | And of bexp list
  | Equal_to_n of exp
  | Equal_to of exp * exp
  | Greater_than of exp * exp
  | Weekday of Time.Day_of_week.t
  | Mensis of Time.Month.t
  | Annus of int
  | Leap

type selector = Selector of Time.interval * bexp * selector list

val selector_of_sexp : Sexp.sexp -> selector
