open Sexp

exception Syntax of string

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

let ios s =
  try int_of_string s with
    _ -> err ~e:("invalid integer: " ^ s) ()

type exp =
  | Variable
  | Var_week_of_month
  | Var_day_of_month
  | Constant of int
  | Modulo of exp * exp
  | Sum of exp list

let rec exp_of_sexp = function
  | Atom "n" -> Variable
  | Atom "week-of-month" -> Var_week_of_month
  | Atom "day-of-month" -> Var_day_of_month
  | Atom i -> Constant (ios i)
  | List (Atom "mod" :: a :: b :: []) -> Modulo (exp_of_sexp a, exp_of_sexp b)
  | List (Atom "+" :: ints) -> Sum (List.map exp_of_sexp ints)
  | List (Atom s :: _) -> expect ["+"; "mod"] s
  | List [] -> err ~e:"empty nth selector" ()
  | _ -> err ()

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

let rec bexp_of_sexp = function
  | List [] -> All
  | List (Atom "not" :: opt :: []) -> Not (bexp_of_sexp opt)
  | List (Atom "or" :: opts) -> Or (List.map bexp_of_sexp opts)
  | List (Atom "and" :: opts) -> And (List.map bexp_of_sexp opts)
  | List (Atom "eq" :: x :: y :: []) -> Equal_to (exp_of_sexp x, exp_of_sexp y)
  | List (Atom "gt" :: x :: y :: []) ->
    Greater_than (exp_of_sexp x, exp_of_sexp y)
  | Atom "leap" -> Leap
  | Atom s as sexp -> begin match Time.Day_of_week.of_string s with
    | Some day -> Weekday day
    | None -> match Time.Month.of_string s with
      | Some m -> Mensis m
      | None ->
        let l = String.length s in
        if l > 2 && s.[0] = 'a' && s.[1] = 'd'
          then Annus (ios (String.sub s 2 (l - 2)))
          else Equal_to_n (exp_of_sexp sexp)
  end
  | sexp -> Equal_to_n (exp_of_sexp sexp)

type selector = Selector of Time.interval * bexp * selector list

let selector_of_sexp sexp =
  let selectors = ["minute"; "hour"; "day"; "week"; "month"; "year"] in
  let rec self old = function
    | List (Atom s :: rest) ->
      let (cond, ss) = begin match rest with
        | Atom _ as opt :: sub -> (opt, sub)
        | List (Atom opt :: _) as opts :: sub
          when not (List.exists ((=) opt) selectors) -> (opts, sub)
        | sub -> (List [], sub)
      end in
      begin match Time.interval_of_string s with
        Some i -> if old <= i || i = Time.Seconds
          then err ~e:("unexpected " ^ s ^ " selector") ()
          else Selector (i, bexp_of_sexp cond, List.map (self i) ss)
        | None -> expect selectors s
      end
    | _ -> err ()
  in
  self Time.Eternity sexp
