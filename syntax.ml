open Core.Std

type exp =
  | Variable
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

let rec sexp_of_exp = function
  | Variable -> Sexp.Atom "n"
  | Constant i -> Int.sexp_of_t i
  | Modulo (a, b) -> Sexp.List [Sexp.Atom "mod"; sexp_of_exp a; sexp_of_exp b]
  | Sum a -> Sexp.List (Sexp.Atom "+" :: List.map a sexp_of_exp)

let rec exp_of_sexp = function
  | Sexp.Atom "n" -> Variable
  | Sexp.Atom i -> Constant (int_of_string i)
  | Sexp.List (Sexp.Atom "mod" :: a :: b :: []) -> Modulo (exp_of_sexp a, exp_of_sexp b)
  | Sexp.List (Sexp.Atom "+" :: ints) -> Sum (List.map ints exp_of_sexp)

type bexp =
  | Nth of exp
  | Equal of exp * exp

let sexp_of_bexp = function
  | Nth exp -> sexp_of_exp exp
  | Equal (x, y) -> Sexp.List [Sexp.Atom "eq"; sexp_of_exp x; sexp_of_exp y]

let bexp_of_sexp = function
  | Sexp.List (Sexp.Atom "eq" :: x :: y :: []) -> Equal (exp_of_sexp x, exp_of_sexp y)
  | sexp -> Nth (exp_of_sexp sexp)

type dayopt =
  | NthDay of bexp
  | Weekday of Day_of_week.t

let sexp_of_dayopt = function
  | NthDay exp -> Sexp.List [Sexp.Atom "nth"; sexp_of_bexp exp]
  | Weekday day -> Sexp.Atom begin match day with
    | Mon -> "mon"
    | Tue -> "tue"
    | Wed -> "wed"
    | Thu -> "thu"
    | Fri -> "fri"
    | Sat -> "sat"
    | Sun -> "sun"
  end

let dayopt_of_sexp = function
  | Sexp.List (Sexp.Atom "nth" :: exp :: []) -> NthDay (bexp_of_sexp exp)
  | Sexp.Atom s -> Weekday begin match s with
    | "mon" -> Mon
    | "tue" -> Tue
    | "wed" -> Wed
    | "thu" -> Thu
    | "fri" -> Fri
    | "sat" -> Sat
    | "sun" -> Sun
  end

type dayopts =
  | IncDay of dayopt list
  | ExclDay of dayopt list

let sexp_of_dayopts opts =
  let f n x = Sexp.List (Sexp.Atom n :: List.map x sexp_of_dayopt) in
  match opts with
    | IncDay x -> f "inc" x
    | ExclDay x -> f "excl" x

let dayopts_of_sexp sexp =
  match sexp with
    | Sexp.List (Sexp.Atom s :: opts) ->
      match s with
        | "inc" -> IncDay (List.map opts dayopt_of_sexp)
        | "excl" -> ExclDay (List.map opts dayopt_of_sexp)

type monthopt =
  | NthMonth of bexp
  | Mensis of Month.t

let sexp_of_monthopt = function
  | NthMonth exp -> Sexp.List [Sexp.Atom "nth"; sexp_of_bexp exp]
  | Mensis m -> Sexp.Atom begin match m with
    | Jan -> "jan" | Feb -> "feb" | Mar -> "mar" | Apr -> "apr"
    | May -> "may" | Jun -> "jun" | Jul -> "jul" | Aug -> "aug"
    | Sep -> "sep" | Oct -> "oct" | Nov -> "nov" | Dec -> "dec"
  end

let monthopt_of_sexp = function
  | Sexp.List (Sexp.Atom "nth" :: exp :: []) -> NthMonth (bexp_of_sexp exp)
  | Sexp.Atom m -> Mensis begin match m with
    | "jan" -> Jan | "feb" -> Feb | "mar" -> Mar | "apr" -> Apr
    | "may" -> May | "jun" -> Jun | "jul" -> Jul | "aug" -> Aug
    | "sep" -> Sep | "oct" -> Oct | "nov" -> Nov | "dec" -> Dec
  end

type monthopts =
  | IncMonth of monthopt list
  | ExclMonth of monthopt list
  | Day of dayopts list

let sexp_of_monthopts = function
  | IncMonth o -> Sexp.List (Sexp.Atom "inc" :: List.map o sexp_of_monthopt)
  | ExclMonth o -> Sexp.List (Sexp.Atom "excl" :: List.map o sexp_of_monthopt)
  | Day o -> Sexp.List (Sexp.Atom "day" :: List.map o sexp_of_dayopts)

let monthopts_of_sexp = function
  | Sexp.List (Sexp.Atom s :: opts) ->
    match s with
      | "inc" -> IncMonth (List.map opts monthopt_of_sexp)
      | "excl" -> ExclMonth (List.map opts monthopt_of_sexp)
      | "day" -> Day (List.map opts dayopts_of_sexp)

type yearopt =
  | NthYear of bexp
  | Annus of int

let sexp_of_yearopt = function
  | NthYear exp -> Sexp.List [Sexp.Atom "nth"; sexp_of_bexp exp]
  | Annus i -> Int.sexp_of_t i

let yearopt_of_sexp sexp =
  match sexp with
    | Sexp.List (Sexp.Atom "nth" :: exp :: []) -> NthYear (bexp_of_sexp exp)
    | Sexp.Atom _ -> Annus (Int.t_of_sexp sexp)

type yearopts =
  | IncYear of yearopt list
  | ExclYear of yearopt list
  | Month of monthopts list
  | Day of dayopts list

let sexp_of_yearopts opts =
  match opts with
    | IncYear o -> Sexp.List (Sexp.Atom "inc" :: List.map o sexp_of_yearopt)
    | ExclYear o -> Sexp.List (Sexp.Atom "excl" :: List.map o sexp_of_yearopt)
    | Month o -> Sexp.List (Sexp.Atom "month" :: List.map o sexp_of_monthopts)
    | Day o -> Sexp.List (Sexp.Atom "day" :: List.map o sexp_of_dayopts)

let yearopts_of_sexp sexp =
  match sexp with
    | Sexp.List (Sexp.Atom s :: opts) ->
      match s with
        | "inc" -> IncYear (List.map opts yearopt_of_sexp)
        | "excl" -> ExclYear (List.map opts yearopt_of_sexp)
        | "month" -> Month (List.map opts monthopts_of_sexp)
        | "day" -> Day (List.map opts dayopts_of_sexp)

type selector =
  | Or of selector list
  | And of selector list
  | Year of yearopts list
  | Month of monthopts list
  | Day of dayopts list

let rec sexp_of_selector = function
  | Or selectors -> Sexp.List (Sexp.Atom "or" :: List.map selectors sexp_of_selector)
  | And selectors -> Sexp.List (Sexp.Atom "and" :: List.map selectors sexp_of_selector)
  | Year opt -> Sexp.List (Sexp.Atom "year" :: List.map opt sexp_of_yearopts)
  | Month opt -> Sexp.List (Sexp.Atom "month" :: List.map opt sexp_of_monthopts)
  | Day opt -> Sexp.List (Sexp.Atom "day" :: List.map opt sexp_of_dayopts)

let rec selector_of_sexp sexp =
  match sexp with
    | Sexp.List (Sexp.Atom s :: opts) ->
      begin match s with
        | "or" -> Or (List.map opts selector_of_sexp)
        | "and" -> And (List.map opts selector_of_sexp)
        | "year" -> Year (List.map opts yearopts_of_sexp)
        | "month" -> Month (List.map opts monthopts_of_sexp)
        | "day" -> Day (List.map opts dayopts_of_sexp)
      end
