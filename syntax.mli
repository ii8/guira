open Core.Std

exception Syntax of string

type exp =
  | Variable
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

type bexp =
  | Nth of exp
  | Equal_to of exp * exp

type dayopt =
  | NthDay of bexp
  | Weekday of Day_of_week.t

type dayopts =
  | IncDay of dayopt list
  | ExclDay of dayopt list

type monthopt =
  | NthMonth of bexp
  | Mensis of Month.t

type monthopts =
  | IncMonth of monthopt list
  | ExclMonth of monthopt list
  | Day of dayopts list

type yearopt =
  | NthYear of bexp
  | Annus of int

type yearopts =
  | IncYear of yearopt list
  | ExclYear of yearopt list
  | Month of monthopts list
  | Day of dayopts list

type selector =
  | Or of selector list
  | And of selector list
  | Year of yearopts list
  | Month of monthopts list
  | Day of dayopts list

val sexp_of_selector : selector -> Sexp.t
val selector_of_sexp : Sexp.t -> selector
