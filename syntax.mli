open Core.Std

type exp =
  | Variable
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

type bexp =
  | Nth of exp
  | Equal of exp * exp

type dayopt =
  | NthDay of bexp
  | Weekday of Day_of_week.t

type dayopts =
  | IncDay of dayopt list
  | ExclDay of dayopt list

type yearopt =
  | NthYear of bexp
  | Annus of int

type yearopts =
  | IncYear of yearopt list
  | Day of dayopts list

type selector =
  | Or of selector list
  | And of selector list
  | Year of yearopts list
  | Day of dayopts list

val sexp_of_selector : selector -> Sexp.t
val selector_of_sexp : Sexp.t -> selector
