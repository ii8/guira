open Core.Std
open Syntax

type interval = Days | Weeks | Months | Years | Eternity

type state = {
  d : Date.t;
  r : Date.t;
  i : interval;
}

let week_start d =
  Date.day_of_week d
  |> Day_of_week.iso_8601_weekday_number
  |> (fun a -> 1 - a)
  |> Date.add_days d

let month_start d = Date.create_exn ~y:(Date.year d) ~m:(Date.month d) ~d:1

let year_start d = Date.create_exn ~y:(Date.year d) ~m:Jan ~d:1

let eval i s expression =
  let first = match s.i with
    | Days -> assert false
    | Weeks -> week_start s.d
    | Months -> month_start s.d
    | Years -> year_start s.d
    | Eternity -> s.r in
  let n = match i with
    | Days -> Date.diff s.d first + 1
    | Weeks -> let a = (Date.diff s.d first + 1) mod 7 in (Date.diff s.d first + a) / 7
    | Months ->
      let m a = Date.month a |> Month.to_int in
      (m s.d - (m first - 1)) + (Date.year s.d - Date.year first) * 12
    | Years -> Date.year s.d - Date.year first
    | Eternity -> assert false in

  let rec ev = function
    | Variable -> n
    | Constant x -> x
    | Modulo (x, y) -> (ev x) mod (ev y)
    | Sum xs -> List.map ~f:ev xs |> List.fold ~init:0 ~f:(+)
  in
  match expression with
    | Nth exp -> ev exp = n
    | Equal_to (a, b) -> ev a = ev b

let rec match_days s pats =
  match pats with
    | [] -> false
    | NthDay exp :: [] -> eval Days s exp
    | NthDay exp :: rest -> if eval Days s exp then true else match_days s rest
    | Weekday day :: [] -> day = Date.day_of_week s.d
    | Weekday day :: rest ->
      if day = Date.day_of_week s.d
        then true
        else match_days s rest

let rec filter_days s op =
  match op with
    | [] -> true
    | IncDay l :: [] -> match_days s l
    | IncDay l :: ls -> if match_days s l then filter_days s ls else false
    | ExclDay l :: [] -> not (match_days s l)
    | ExclDay l :: ls -> if match_days s l then false else filter_days s ls

let rec match_months s pats =
  match pats with
    | [] -> false
    | NthMonth exp :: [] -> eval Months s exp
    | NthMonth exp :: rest -> if eval Months s exp then true else match_months s rest
    | Mensis m :: [] -> m = Date.month s.d
    | Mensis m :: rest -> if m = Date.month s.d then true else match_months s rest

let rec filter_months s op =
  match op with
    | [] -> true
    | IncMonth l :: [] -> match_months s l
    | IncMonth l :: ls -> if match_months s l then filter_months s ls else false
    | ExclMonth l :: [] -> not (match_months s l)
    | ExclMonth l :: ls -> if match_months s l then false else filter_months s ls
    | Day opts :: [] -> filter_days { s with i = Months } opts
    | _ -> assert false

let rec match_years s pats =
  match pats with
    | [] -> false
    | NthYear exp :: [] -> eval Years s exp
    | NthYear exp :: rest -> if eval Years s exp then true else match_years s rest
    | Annus i :: [] -> i = Date.year s.d
    | Annus i :: rest -> if i = Date.year s.d then true else match_years s rest

let rec filter_years s op =
  match op with
    | [] -> true
    | IncYear l :: [] -> match_years s l
    | IncYear l :: ls -> if match_years s l then filter_years s ls else false
    | ExclYear l :: [] -> not (match_years s l)
    | ExclYear l :: ls -> if match_years s l then false else filter_years s ls
    | Month opts :: [] -> filter_months { s with i = Years } opts
    | Day opts :: [] -> filter_days { s with i = Years } opts
    | _ -> assert false

let rec filter selector d r =
  let f s = filter s d r in
  let s = { d; r; i = Eternity } in
  match selector with
    | Or fs -> List.exists (List.map ~f fs) ~f:(fun i -> i)
    | And fs -> List.for_all (List.map ~f fs) ~f:(fun i -> i)
    | Year opts -> filter_years s opts
    | Month opts -> filter_months s opts
    | Day opts -> filter_days s opts
