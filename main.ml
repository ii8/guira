
type options = {
  mutable date : Time.t option;
  mutable start : Time.t;
  mutable last : Time.t;
  mutable fmt : string;
  mutable interval : Time.interval;
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
    -f  --format FORMAT                   \
    strftime output format\n  \
    -i  --interval (minute|hour|day|week|month|year)\
    \n                                        \
    interval at which dates are queried\n  \
    -h  --help                            \
    print this help and exit\
  "

let list_dates sdate edate selector fmt interval =
  let check d =
    if Filter.filter selector d sdate interval
      then print_endline (Time.format d fmt) in

  let rs = Time.tfloor sdate interval in
  let re = Time.tfloor edate interval in
  let rec loop d =
    if re >= d
      then begin
        check d;
        loop (Time.next d interval)
      end in

  loop rs

let run o =
  let selector = try Sexp.parse_stdin () |> Syntax.selector_of_sexp with
    | a -> begin match a with
      | Syntax.Syntax e -> prerr_endline ("Error: " ^ e)
      | Failure "int_of_string" -> prerr_endline "Error: bad integer"
      | Failure e -> prerr_endline ("Error: " ^ e)
      | _ -> prerr_endline "Error: invalid expression"
    end; exit 2 in

  match o.date with
    | None -> list_dates o.start o.last selector o.fmt o.interval
    | Some d ->
      exit ( if Filter.filter selector d o.start o.interval then 0 else 1)

let () =
  let get_date opt =
    try Time.of_string opt with
      | _ ->
        prerr_endline ("Error: invalid date string '" ^ opt ^ "'"); exit 2 in

  let o = {
    date = None;
    start = Time.now ();
    last = Time.create ~day:31 ~month:Time.Month.Dec 9999;
    fmt = "%F";
    interval = Time.Days;
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
  let set_fmt opt = o.fmt <- opt in
  let set_interval opt =
    o.interval <- match Time.interval_of_string opt with
      | Some i -> i
      | None ->
        prerr_endline ("Error: invalid interval '" ^ opt ^ "'");
        exit 2 in

  Array.iter (fun opt ->
    match opt with
      | "-s" | "--start-date" -> current := set_start
      | "-e" | "--end-date" -> current := set_end
      | "-f" | "--format" -> current := set_fmt
      | "-i" | "--interval" -> current := set_interval
      | "-h" | "--help" -> o.help <- true
      | _ -> begin match String.get opt 0 with
        | '-' -> noop opt
        | _ -> !current opt; current := set_date
      end
  ) Sys.argv;

  if o.help
    then usage ()
    else run o
