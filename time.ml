
type interval
  = Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
  | Months
  | Years
  | Eternity

let interval_of_string = function
  | "second" -> Some Seconds
  | "minute" -> Some Minutes
  | "hour"   -> Some Hours
  | "day"    -> Some Days
  | "week"   -> Some Weeks
  | "month"  -> Some Months
  | "year"   -> Some Years
  | _ -> None

module Month = struct
  type t
    = Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec

  let rec of_int = function
    | 1  -> Jan | 2  -> Feb | 3  -> Mar | 4  -> Apr
    | 5  -> May | 6  -> Jun | 7  -> Jul | 8  -> Aug
    | 9  -> Sep | 10 -> Oct | 11 -> Nov | 12 -> Dec
    | 0  -> Dec
    | a  -> of_int (a mod 12)

  let to_int = function
    | Jan -> 1  | Feb -> 2  | Mar -> 3  | Apr -> 4
    | May -> 5  | Jun -> 6  | Jul -> 7  | Aug -> 8
    | Sep -> 9  | Oct -> 10 | Nov -> 11 | Dec -> 12

  let of_string = function
    | "jan" -> Some Jan | "feb" -> Some Feb | "mar" -> Some Mar
    | "apr" -> Some Apr | "may" -> Some May | "jun" -> Some Jun
    | "jul" -> Some Jul | "aug" -> Some Aug | "sep" -> Some Sep
    | "oct" -> Some Oct | "nov" -> Some Nov | "dec" -> Some Dec
    | _ -> None

  let to_string = function
    | Jan -> "jan" | Feb -> "feb" | Mar -> "mar" | Apr -> "apr"
    | May -> "may" | Jun -> "jun" | Jul -> "jul" | Aug -> "aug"
    | Sep -> "sep" | Oct -> "oct" | Nov -> "nov" | Dec -> "dec"

  let next m = let n = to_int m + 1 in of_int n
  let last m = let n = to_int m - 1 in of_int n
end
open Month

module Day_of_week = struct
  type t
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun

  let rec of_int = function
    | 1 -> Mon | 2 -> Tue | 3 -> Wed | 4 -> Thu
    | 5 -> Fri | 6 -> Sat | 7 -> Sun | 0 -> Sun | a -> of_int (a mod 7)

  let to_int = function
    | Mon -> 1 | Tue -> 2 | Wed -> 3 | Thu -> 4
    | Fri -> 5 | Sat -> 6 | Sun -> 7

  let of_string = function
    | "mon" -> Some Mon | "tue" -> Some Tue | "wed" -> Some Wed
    | "thu" -> Some Thu | "fri" -> Some Fri | "sat" -> Some Sat
    | "sun" -> Some Sun | _ -> None

  let to_string = function
    | Mon -> "mon" | Tue -> "tue" | Wed -> "wed" | Thu -> "thu"
    | Fri -> "fri" | Sat -> "sat" | Sun -> "sun"
end
open Day_of_week

type t = {
  year : int;
  month : Month.t;
  day : int;
  hour : int;
  minute : int;
  second : int;
}

let year t = t.year
let month t = t.month
let day t = t.day
let hour t = t.hour
let minute t = t.minute
let second t = t.second

let leap year =
  year mod 4 = 0 && year mod 100 <> 0 || year mod 400 = 0

let days_in_month year = function
  | Month.Feb -> if leap year then 29 else 28
  | Month.Apr | Month.Jun | Month.Sep | Month.Nov -> 30
  | Month.Jan | Month.Mar | Month.May | Month.Jul
  | Month.Aug | Month.Oct | Month.Dec -> 31

let create ?second:(second = 0) ?minute:(minute = 0) ?hour:(hour = 0)
           ?day:(day = 1) ?month:(month = Month.Jan) year =
  { year; month; day; hour; minute; second }

let now () = Unix.time () |> Unix.gmtime |> fun tm -> create
  ~second:tm.Unix.tm_sec
  ~minute:tm.Unix.tm_min
  ~hour:tm.Unix.tm_hour
  ~day:tm.Unix.tm_mday
  ~month:(Month.of_int (tm.Unix.tm_mon + 1))
  (tm.Unix.tm_year + 1900)

let valid t =
  if t.year < 0 then failwith "year cannot be less than 0";
  if t.day < 1 then failwith "day must be positive";
  if t.day > days_in_month t.year t.month then failwith "day too great";
  if t.hour < 0 || t.hour > 23 then failwith "hour must be between 0 and 23";
  if t.minute < 0 || t.minute > 59 then failwith "hours only have 60 minutes";
  if t.second < 0 || t.second > 59 then failwith "invalid second";
  t

let rec next date interval =
  let add_days i =
    let num_days = days_in_month date.year date.month in
    let j = date.day + i in
    if num_days < j
      then { (next date Months) with day = j - num_days }
      else { date with day = j }
  in
  match interval with
    | Seconds ->
      let new_sec = date.second + 1 in
      if new_sec = 60
        then { (next date Minutes) with second = 0 }
        else { date with second = new_sec }
    | Minutes ->
      let new_min = date.minute + 1 in
      if new_min = 60
        then { (next date Hours) with minute = 0 }
        else { date with minute = new_min }
    | Hours ->
      let new_hour = date.hour + 1 in
      if new_hour = 24
        then { (add_days 1) with hour = 0 }
        else { date with hour = new_hour }
    | Days -> add_days 1
    | Weeks -> add_days 7
    | Months ->
      let (new_year, new_month) = match date.month with
        | Month.Dec -> (date.year + 1, Month.Jan)
        | m -> (date.year, Month.next m) in
      { date with
        year = new_year;
        month = new_month;
        day = min (days_in_month new_year new_month) date.day }
    | Years ->
      if date.month = Month.Feb
         && date.day = 29
         && date.year + 1 |> leap |> not
        then { date with year = date.year + 1; day = 28 }
        else { date with year = date.year + 1 }
    | Eternity -> create ~second:59 ~minute:59 ~hour:23 ~day:31 ~month:Dec 9999

let day_of_week =
  let table = [| 0; 3; 2; 5; 0; 3; 5; 1; 4; 6; 2; 4 |] in
  (fun t ->
    let y = if t.month < Month.Mar then (year t) - 1 else (year t) in
    Day_of_week.of_int
    ((y + y / 4 - y / 100 + y / 400
      + table.(Month.to_int t.month - 1)
      + t.day) mod 7))

let rata_die t =
  let (y, m) =
    if t.month < Mar
      then t.year - 1, Month.to_int t.month + 12
      else t.year, Month.to_int t.month in
  365 * y + y / 4 - y / 100 + y / 400 + (153 * m - 457) / 5 + t.day - 306

let diff a b = rata_die a - rata_die b

let this_monday t =
  let i = day_of_week t |> Day_of_week.to_int |> (-) (t.day + 1) in
  if i < 0
    then if t.month = Jan
      then let new_year = t.year - 1 in { t with
        year = new_year;
        month = Dec;
        day = 31 + i }
      else let new_month = Month.last t.month in { t with
        month = new_month;
        day = days_in_month t.year new_month + i }
    else { t with day = i }

let this_thursday t =
  let f = fun a -> next a Days in
  t |> this_monday |> f |> f |> f

let tfloor t ?eternity:(e = create 0) = function
  | Seconds -> t
  | Minutes -> { t with second = 0 }
  | Hours -> { t with second = 0; minute = 0 }
  | Days -> { t with second = 0; minute = 0; hour = 0 }
  | Weeks -> let u = this_monday t in create ~day:u.day ~month:u.month u.year
  | Months -> create ~month:t.month t.year
  | Years -> create t.year
  | Eternity -> e

let first_week t =
  let rec loop u i = if i = 0 then u else loop (next u Days) (i - 1) in
  day_of_week t
  |> Day_of_week.to_int
  |> (-) (Day_of_week.to_int Thu)
  |> (fun d -> if d < 0 then d + 7 else d)
  |> loop t
  |> this_monday

let week_of_month t =
  (this_thursday t).day / 7 + 1

let of_string s =
  let y () = int_of_string (String.sub s 0 4) in
  let m () =
    let m = int_of_string (String.sub s 5 2) in
    if m > 12 || m < 1
      then failwith "month must be between 1 and 12"
      else Month.of_int m in
  let p b = int_of_string (String.sub s b 2) in
  valid @@ match String.length s with
    | 4 -> create (y ())
    | 7 -> create ~month:(m ()) (y ())
    | 10 -> create ~day:(p 8) ~month:(m ()) (y ())
    | 13 -> create ~hour:(p 11) ~day:(p 8) ~month:(m ()) (y ())
    | 16 -> create ~minute:(p 14) ~hour:(p 11) ~day:(p 8) ~month:(m ()) (y ())
    | l when l >= 19 -> create ~second:(p 17) ~minute:(p 14) ~hour:(p 11)
                               ~day:(p 8) ~month:(m ()) (y ())
    | _ -> failwith "invalid date string"

let to_string t =
  Printf.sprintf "%04u-%02u-%02u" t.year (Month.to_int t.month) t.day

let format t fmt =
  let res = ref [] in
  let a s = res := s :: !res in
  let on = ref false in

  let add0 i = a (if i < 10 then "0" ^ string_of_int i else string_of_int i) in
  String.iter begin fun c ->
    if !on
      then begin on := false; match c with
        | 'a' -> a (Day_of_week.to_string (day_of_week t))
        | 'b' -> a (Month.to_string t.month)
        | 'd' -> add0 t.day
        | 'F' -> a (to_string t)
        | 'H' -> add0 t.hour
        | 'm' -> add0 (Month.to_int t.month)
        | 'M' -> add0 t.minute
        | 'S' -> add0 t.second
        | 'Y' -> a (string_of_int t.year)
        | '%' -> a "%"
        | _ -> a ("%" ^ String.make 1 c)
      end else match c with
        | '%' -> on := true
        | _ -> a (String.make 1 c)
  end fmt;
  String.concat "" (List.rev !res)

(* Tests *)

let test_leap () =
  List.for_all (fun a -> a) [
    not (leap 1900);
    leap 1904;
    not (leap 1917);
    leap 1964;
    leap 2000;
    leap 2016;
    leap 2072;
    not (leap 2100);
  ]

let test_now () =
  diff (now ()) (create 2016) > 0

let test_next () =
  let c = create in
  let last_day = c ~second:59 ~minute:59 ~hour:23 ~day:31 ~month:Dec 1999 in
  List.for_all (fun a -> a) [
    next last_day Seconds = c 2000;
    next last_day Months = c ~second:59 ~minute:59 ~hour:23 ~day:31 2000;
    next last_day Years = { last_day with year = 2000 };
    next (c ~day:21 ~month:Jul 2017) Days = c ~day:22 ~month:Jul 2017;
    next (c ~day:29 ~month:Feb 2016) Years = c ~day:28 ~month:Feb 2017;
    next last_day Weeks = c ~second:59 ~minute:59 ~hour:23 ~day:7 2000;
  ]

let test_day_of_week () =
  let mk day month year = create ~day ~month year in
  List.for_all (fun a -> a) [
    day_of_week (mk 11 Jul 2016) = Mon;
    day_of_week (mk 1 Oct 1993) = Fri;
    day_of_week (mk 27 Mar 1827) = Tue;
    day_of_week (mk 31 Dec 2412) = Mon;
    day_of_week (mk 1 Mar 2000) = Wed;
    day_of_week (mk 2 Jan 2000) = Sun;
  ]

let test_diff () =
  let mk day month year = create ~day ~month year in
  List.for_all (fun a -> a) [
    diff (mk 2 Jul 1993) (mk 2 Jul 1993) = 0;
    diff (mk 23 Dec 2001) (mk 22 Dec 2001) = 1;
    diff (mk 6 Aug 1943) (mk 2 Jan 2200) = 0 - 93_652;
    diff (mk 31 Mar 2014) (mk 11 Dec 2013) = 110;
    diff (mk 15 Jun 3065) (mk 1 Jan 2016) = 383_305;
  ]

let test_tfloor () =
  tfloor (create ~hour:14 ~day:15 ~month:Jul 2016) Weeks =
    create ~day:11 ~month:Jul 2016

let test_this_monday () =
  List.for_all (fun a -> a) [
    this_monday (create ~day:7 2000) = (create ~day:3 2000);
    this_monday (create ~month:May 1445) = (create ~day:28 ~month:Apr 1445);
    this_monday (create ~day:2 2000) = (create ~day:27 ~month:Dec 1999);
  ]

let test_first_week () =
  let c d m y = create ~day:d ~month:m y in
  List.for_all (fun a -> a) [
    first_week (c 1 Jan 2017) = (c 2 Jan 2017);
    first_week (c 1 Jan 2021) = (c 4 Jan 2021);
    first_week (c 1 Nov 2021) = (c 1 Nov 2021);
    first_week (c 1 Sep 2022) = (c 29 Aug 2022);
  ]

let tests = [
    "Time.leap", test_leap;
    "Time.now", test_now;
    "Time.next", test_next;
    "Time.day_of_week", test_day_of_week;
    "Time.diff", test_diff;
    "Time.tfloor", test_tfloor;
    "Time.this_monday", test_this_monday;
    "Time.first_week", test_first_week;
  ]
