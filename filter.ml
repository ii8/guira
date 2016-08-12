open Syntax
open Time

type state = {
  d : Time.t;
  r : Time.t;
  i : interval;
}

let calc_n i s =
  let first = tfloor s.d ~eternity:s.r s.i in
  match i with
    | Seconds -> assert false
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
    | Eternity -> assert false

let ev i s exp =
  let rec f = function
    | Variable -> calc_n i s
    | Var_week_of_month -> week_of_month s.d
    | Var_day_of_month -> day s.d
    | Constant x -> x
    | Modulo (x, y) -> (f x) mod (f y)
    | Sum xs -> List.map f xs |> List.fold_left (+) 0
  in f exp

let eval i s exp =
  let d = if i = Weeks then this_thursday s.d else s.d in
  let rec f = function
    | All -> true
    | Not opt -> not (f opt)
    | Or opts -> List.exists f opts
    | And opts -> List.for_all f opts
    | Equal_to_n exp -> ev i s exp = calc_n i s
    | Equal_to (a, b) -> ev i s a = ev i s b
    | Greater_than (a, b) -> ev i s a > ev i s b
    | Weekday day -> day = day_of_week s.d
    | Mensis m -> m = month d
    | Annus y -> y = year d
    | Leap -> leap (year d)
  in f exp

let filter r p selector d =
  let rec f s (Selector (i, cond, ss)) =
    if p > i
      then true
      else match ss with
        [] -> eval i s cond
        | sub -> if eval i s cond
          then List.exists (f { s with i }) sub
          else false
  in f { d; r; i = Eternity } selector
