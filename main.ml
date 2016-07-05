open Core.Std

type interval = Day | Week | Month | Year

type options = {
  mutable date : Date.t option;
  mutable start : Date.t;
  mutable last : Date.t;
  mutable fmt : string;
  mutable interval : interval;
  mutable help : bool
}

let usage () = print_endline "\
  Usage: guira [OPTION...] [DATE]\n\n\
  s-expression language to query dates\n\n\
  Options:\n  \
    -s  --start-date DATE                 first day to query (today by default)\n  \
    -e  --end-date DATE                   last day to query\n  \
    -f  --format FORMAT                   strftime output format\n  \
    -i  --interval (day|week|month|year)  interval at which dates are queried\n  \
    -h  --help                            print this help and exit\
  "

let list_dates sdate edate selector fmt interval =
  let check d =
    if Filter.filter selector d sdate
      then print_endline (Date.format d fmt) in

  let rec loop d =
    if edate >= d
      then begin
        check d;
        loop begin
          try match interval with
            | Day -> Date.add_days d 1
            | Week -> Date.add_days d 7
            | Month -> Date.add_months d 1
            | Year -> Date.add_months d 12
          with _ -> exit 0
        end
      end in

  loop sdate

let run o =
  let sexp = Sexp.input_sexp stdin in
  let selector = try Syntax.selector_of_sexp sexp with
    | a -> begin match a with
      | Syntax.Syntax e -> prerr_endline ("Error: " ^ e)
      | Failure "int_of_string" -> prerr_endline "Error: bad integer"
      | _ -> prerr_endline "Error: invalid expression"
    end; exit 2 in

  match o.date with
    | None -> list_dates o.start o.last selector o.fmt o.interval
    | Some d -> exit (if Filter.filter selector d o.start then 0 else 1)

let () =
  let get_date opt =
    try Date.of_string opt with
      | _ ->
        prerr_endline ("Error: invalid date string '" ^ opt ^ "'"); exit 2 in

  let o = {
    date = None;
    start = Date.today ~zone:Core.Zone.local;
    last = Date.create_exn ~y:9999 ~m:Month.Dec ~d:31;
    fmt = "%F";
    interval = Day;
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
    o.interval <- match opt with
      | "day" -> Day
      | "week" -> Week
      | "month" -> Month
      | "year" -> Year
      | _ ->
        prerr_endline ("Error: invalid interval '" ^ opt ^ "'");
        exit 2 in

  ignore @@ Array.map ~f:(fun opt ->
    match opt with
      | "-s" | "--start-date" -> current := set_start
      | "-e" | "--end-date" -> current := set_end
      | "-f" | "--format" -> current := set_fmt
      | "-i" | "--interval" -> current := set_interval
      | "-h" | "--help" -> o.help <- true
      | _ -> begin match String.to_list opt with
        | '-' :: _ -> noop opt
        | _ -> !current opt; current := set_date
      end
  ) Sys.argv;

  if o.help
    then usage ()
    else run o
