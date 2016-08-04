
type options = {
  mutable date : Time.t option;
  mutable start : Time.t;
  mutable last : Time.t;
  mutable interval : Time.interval option;
  mutable fmt : string option;
  mutable help : bool
}

let usage () = print_endline "\
  Usage: guira [OPTION...] [DATE]\n\n\
  s-expression language to query dates\n\n\
  Options:\n  \
    -s  --start-date DATE                 \
    first day to query (today by default)\n  \
    -e  --end-date DATE                   \
    last day to query\n  \
    -i  --interval (minute|hour|day|week|month|year)\
    \n                                        \
    interval at which dates are queried\n  \
    -f  --format FORMAT                   \
    strftime output format\n  \
    -h  --help                            \
    print this help and exit\
  "; exit 0

let rec min = function
  [] -> assert false
  | a::[] -> a
  | a::b -> let mb = min b in if a < mb then a else mb

let rec find_precision = function
  Syntax.Selector (i, _, []) -> i
  | Syntax.Selector (_, _, a) -> min (List.map find_precision a)

let format_for_interval = function
  | Time.Seconds -> "%Y-%m-%d %H:%M:%S"
  | Time.Minutes | Time.Hours -> "%Y-%m-%d %H:%M"
  | Time.Days | Time.Weeks -> "%Y-%m-%d"
  | Time.Months -> "%Y-%m"
  | Time.Years -> "%Y"
  | Time.Eternity -> assert false

let list_dates sdate edate selector fmt interval =
  let check d =
    let week_date d =
      if interval = Time.Weeks
        then Time.this_monday d
        else d in
    if Filter.filter sdate interval selector d
      then print_endline (Time.format (week_date d) fmt) in

  let f d =
    let f = Time.tfloor d interval in
    if interval = Time.Weeks
      then Time.next (Time.next (Time.next f Time.Days) Time.Days) Time.Days
      else f in

  let rs = f sdate in
  let re = f edate in
  let rec loop d =
    if re >= d
      then begin
        check d;
        loop (Time.next d interval)
      end in

  loop rs

let parse () =
  try Sexp.parse_stdin () |> Syntax.selector_of_sexp with
    | a -> begin match a with
      | Syntax.Syntax e -> prerr_endline ("Error: " ^ e)
      | Failure "int_of_string" -> prerr_endline "Error: bad integer"
      | Failure e -> prerr_endline ("Error: " ^ e)
      | _ -> prerr_endline "Error: invalid expression"
    end; exit 2

let () =
  let get_date opt =
    try Time.of_string opt with
      | _ ->
        prerr_endline ("Error: invalid date string '" ^ opt ^ "'"); exit 2 in

  let o = {
    date = None;
    start = Time.now ();
    last = Time.create ~day:31 ~month:Time.Month.Dec 9999;
    interval = None;
    fmt = None;
    help = false;
  } in

  let prog_name _ = () in
  let current = ref prog_name in

  let noop opt = prerr_endline ("Warning: option '" ^ opt ^ "' ignored") in
  let set_date opt =
    match o.date with
      | None -> o.date <- Some (get_date opt)
      | Some _ -> noop opt in
  let set_start opt = o.start <- get_date opt in
  let set_end opt = o.last <- get_date opt in
  let set_interval opt =
    o.interval <- match Time.interval_of_string opt with
      | None -> prerr_endline ("Error: invalid interval '" ^ opt ^ "'"); exit 2
      | i -> i in
  let set_fmt opt = o.fmt <- Some opt in

  Array.iter (fun opt ->
    match opt with
      | "-s" | "--start-date" -> current := set_start
      | "-e" | "--end-date" -> current := set_end
      | "-f" | "--format" -> current := set_fmt
      | "-i" | "--interval" -> current := set_interval
      | "-h" | "--help" -> o.help <- true
      | "" -> ()
      | _ -> begin match String.get opt 0 with
        | '-' -> noop opt
        | _ -> !current opt; current := set_date
      end
  ) Sys.argv;

  if o.help
    then usage ();

  let s = parse () in

  let interval = match o.interval with
    None -> find_precision s
    | Some i -> i in

  let fmt = match o.fmt with
    None -> format_for_interval interval
    | Some f -> f in

  match o.date with
    | None -> list_dates o.start o.last s fmt interval
    | Some d -> exit (if Filter.filter o.start interval s d then 0 else 1)
