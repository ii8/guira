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
    | Minutes ->
      (diff s.d first) * 24 * 60 + 1
      + (hour s.d - hour first) * 60
      + (minute s.d - minute first)
    | Hours -> (diff s.d first) * 24 + 1 + hour s.d - hour first
    | Days -> diff s.d first + 1
    | Weeks ->
      let fw = first_week first in
      (diff s.d fw + 7) / 7
    | Months ->
      let m a = month a |> Month.to_int in
      (m s.d - (m first - 1)) + (year s.d - year first) * 12
    | Years -> year s.d - year first + 1
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

let rec filter_minutes s = function
  | Opt (Minuta m) -> m = minute s.d
  | a -> filter_any Minutes filter_minutes s a

let rec filter_hours s = function
  | Opt (Hora h) -> h = hour s.d
  | a -> filter_any Hours filter_hours s a

let rec filter_days s = function
  | Opt (Weekday day) -> day = day_of_week s.d
  | a -> filter_any Days filter_days s a

let rec filter_weeks s opt = filter_any Weeks filter_weeks s opt

let rec filter_months s = function
  | Opt (Mensis m) -> m = month s.d
  | a -> filter_any Months filter_months s a

let rec filter_years s = function
  | Opt Leap -> leap (year s.d)
  | Opt (Annus y) -> y = year s.d
  | a -> filter_any Years filter_years s a

let filter r p selector d =
  let rec run_filter : 'a.
    interval -> (state -> 'a -> bool) -> 'a -> state -> selector list -> bool =
  fun interval ff opt s ss ->
    if p > interval
      then true
      else match ss with
        [] -> ff s opt
        | sub -> if ff s opt
          then List.exists (f { s with i = interval }) sub
          else false
  and f s = function
    | Minute (o, ss) -> run_filter Minutes filter_minutes o s ss
    | Hour   (o, ss) -> run_filter Hours filter_hours o s ss
    | Day    (o, ss) -> run_filter Days filter_days o s ss
    | Week   (o, ss) -> run_filter Weeks filter_weeks o s ss
    | Month  (o, ss) -> run_filter Months filter_months o s ss
    | Year   (o, ss) -> run_filter Years filter_years o s ss
  in f { d; r; i = Eternity } selector


(* Tests *)

let test_eval () =
  let mod3 = Equal_to (Modulo (Variable, Constant 3), Constant 0) in
  List.for_all (fun a -> a) [
    eval 1 (Equal_to_n Variable);
    not (eval 5 mod3);
    eval 6 mod3;
    eval 13 (Equal_to_n (Sum [Constant 4; Constant 7; Constant 2]));
    eval 4 (Greater_than (Variable, Constant 2));
    not @@ eval 4 (Greater_than (Variable, Constant 4));
  ]

let tests = [
    "Filter.eval", test_eval;
  ]
