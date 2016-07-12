open Sexp
module M = Time.Month
module W = Time.Day_of_week

exception Syntax of string

type interval = Days | Weeks | Months | Years | Eternity

let err ?e () =
  let s = match e with None -> "invalid expression" | Some a -> a in
  raise (Syntax s)

let expect want got =
  let s = match want with
    | s :: [] -> s
    | s :: ss ->
      let f a b = a ^ ", '" ^ b ^ "'" in
      "one of " ^ (List.fold_left f ("'" ^ s ^ "'") ss)
    | _ -> "nothing" in
  err ~e:("expected " ^ s ^ " but got '" ^ got ^ "'") ()

type exp =
  | Variable
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

let rec sexp_of_exp = function
  | Variable -> Atom "n"
  | Constant i -> Atom (string_of_int i)
  | Modulo (a, b) -> List [Atom "mod"; sexp_of_exp a; sexp_of_exp b]
  | Sum a -> List (Atom "+" :: List.map sexp_of_exp a)

let rec exp_of_sexp = function
  | Atom "n" -> Variable
  | Atom i -> Constant (int_of_string i)
  | List (Atom "mod" :: a :: b :: []) -> Modulo (exp_of_sexp a, exp_of_sexp b)
  | List (Atom "+" :: ints) -> Sum (List.map exp_of_sexp ints)
  | List (Atom s :: _) -> expect ["+"; "mod"] s
  | List [] -> err ~e:"empty nth selector" ()
  | _ -> err ()

type bexp =
  | Equal_to_n of exp
  | Equal_to of exp * exp
  | Greater_than of exp * exp

let sexp_of_bexp = function
  | Equal_to_n exp -> sexp_of_exp exp
  | Equal_to (x, y) -> List [Atom "eq"; sexp_of_exp x; sexp_of_exp y]
  | Greater_than (x, y) -> List [Atom "gt"; sexp_of_exp x; sexp_of_exp y]

let bexp_of_sexp = function
  | List (Atom "eq" :: x :: y :: []) -> Equal_to (exp_of_sexp x, exp_of_sexp y)
  | List (Atom "gt" :: x :: y :: []) ->
    Greater_than (exp_of_sexp x, exp_of_sexp y)
  | sexp -> Equal_to_n (exp_of_sexp sexp)

type dayopt =
  | Weekday of Time.Day_of_week.t

let dayopt_of_sexp = function
  | Atom s -> Weekday begin match s with
    | "mon" -> W.Mon
    | "tue" -> W.Tue
    | "wed" -> W.Wed
    | "thu" -> W.Thu
    | "fri" -> W.Fri
    | "sat" -> W.Sat
    | "sun" -> W.Sun
    | s -> expect ["a weekday"] s
  end
  | _ -> err ()

type monthopt =
  | Mensis of Time.Month.t

let monthopt_of_sexp = function
  | Atom m ->
    Mensis begin match m with
      | "jan" -> M.Jan | "feb" -> M.Feb | "mar" -> M.Mar | "apr" -> M.Apr
      | "may" -> M.May | "jun" -> M.Jun | "jul" -> M.Jul | "aug" -> M.Aug
      | "sep" -> M.Sep | "oct" -> M.Oct | "nov" -> M.Nov | "dec" -> M.Dec
      | s -> expect ["a month"] s
    end
  | _ -> err ()

type yearopt =
  | Annus of int

let yearopt_of_sexp = function
  | Atom s -> Annus (int_of_string s)
  | _ -> err ()

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

let rec anyopt_of_sexp f = function
  | List [] -> All
  | List (Atom "not" :: opt :: []) -> Not (anyopt_of_sexp f opt)
  | List (Atom "or" :: opts) -> Or (List.map (anyopt_of_sexp f) opts)
  | List (Atom "and" :: opts) -> And (List.map (anyopt_of_sexp f) opts)
  | List (Atom "nth" :: exp :: []) -> Nth (bexp_of_sexp exp, None)
  | a -> Opt (f a)

let selector_of_sexp sexp =
  let selectors = ["day"; "month"; "year"] in
  let rec self old = function
    | List (Atom s :: rest) ->
      let split = begin match rest with
        | Atom _ as opt :: sub -> (opt, sub)
        | List (Atom opt :: _) as opts :: sub
          when not (List.exists ((=) opt) selectors) -> (opts, sub)
        | sub -> (List [], sub)
      end in
      let mk1 f p = anyopt_of_sexp f (fst p) in
      let mk2 n p = List.map (self n) (snd p) in
      begin match s with
        | "day" ->
          if old <= Days
            then err ~e:"unexpected day selector" ()
            else Day (mk1 dayopt_of_sexp split, mk2 Days split)
        | "month" ->
          if old <= Months
            then err ~e:"unexpected month selector" ()
            else Month (mk1 monthopt_of_sexp split, mk2 Months split)
        | "year" ->
          if old <= Years
            then err ~e:"unexpected year selector" ()
            else Year (mk1 yearopt_of_sexp split, mk2 Years split)
        | s -> expect selectors s
      end
    | _ -> err ()
  in
  self Eternity sexp
