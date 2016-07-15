open Syntax
open Time

type state = {
  d : Time.t;
  r : Time.t;
  i : interval;
}

let eval n expression =
  let rec ev = function
    | Variable -> n
    | Constant x -> x
    | Modulo (x, y) -> (ev x) mod (ev y)
    | Sum xs -> List.map ev xs |> List.fold_left (+) 0
  in
  match expression with
    | Equal_to_n exp -> ev exp = n
    | Equal_to (a, b) -> ev a = ev b
    | Greater_than (a, b) -> ev a > ev b

let eval_one i s expression =
  let first = tfloor s.d ~eternity:s.r s.i in
  let n = match i with
    | Days -> diff s.d first + 1
    | Weeks ->
      let a = (diff s.d first + 1) mod 7 in
      (diff s.d first + a) / 7
    | Months ->
      let m a = month a |> Month.to_int in
      (m s.d - (m first - 1)) + (year s.d - year first) * 12
    | Years -> year s.d - year first
    | Eternity -> assert false in
  eval n expression

let eval_list i f s expression opt =
  let first = tfloor s.d ~eternity:(tfloor s.r i) s.i in
  let past_last = next first s.i in
  let rounded = tfloor s.d i in
  let rec loop n tmp res =
    if tmp >= past_last
      then res
      else if f { s with d = tmp } opt
        then
          if eval n expression
            then loop (n + 1) (next tmp i) (tmp :: res)
            else loop (n + 1) (next tmp i) res
        else
          loop n (next tmp i) res
  in
  List.exists ((=) rounded) (loop 1 first [])

let filter_any i f s = function
  | All -> true
  | Not opt -> not (f s opt)
  | Or opts -> List.exists (f s) opts
  | And opts -> List.for_all (f s) opts
  | Nth (exp, selector) ->
    begin match selector with
      | None -> eval_one i s exp
      | Some opt -> eval_list i f s exp opt
    end
  | Opt _ -> assert false

let rec filter_days s = function
  | Opt (Weekday day) -> day = day_of_week s.d
  | a -> filter_any Days filter_days s a

let rec filter_months s = function
  | Opt (Mensis m) -> m = month s.d
  | a -> filter_any Months filter_months s a

let rec filter_years s = function
  | Opt (Annus y) -> y = year s.d
  | a -> filter_any Years filter_years s a

let filter selector d r =
  let rec f s = function
    | Day (opt, []) -> filter_days s opt
    | Day (opt, sub) -> if filter_days s opt
      then List.exists (f { s with i = Days }) sub
      else false
    | Month (opt, []) -> filter_months s opt
    | Month (opt, sub) -> if filter_months s opt
      then List.exists (f { s with i = Months }) sub
      else false
    | Year (opt, []) -> filter_years s opt
    | Year (opt, sub) -> if filter_years s opt
      then List.exists (f { s with i = Years }) sub
      else false
  in f { d; r; i = Eternity } selector
