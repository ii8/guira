open Core.Std

let list_dates sdate edate selector fmt =
  let run d =
    if Filter.filter selector d sdate
      then print_endline (Date.format d fmt) in

  let rec loop d =
    if edate > d
      then (run d; loop (Date.add_days d 1)) in

  loop sdate

let command =
  let now = Date.today Core.Zone.local in
  let last_day = Date.create_exn ~y:9999 ~m:Dec ~d:31 in
  Command.basic
    ~summary:"s-expression language to query dates"
    Command.Spec.(
      empty
      +> flag "-s" (optional_with_default now date) ~doc:"DATE start date (defaults to today)"
      +> flag "-e" (optional_with_default last_day date) ~doc:"DATE end date"
      +> flag "-f" (optional_with_default "%F" string) ~doc:"FORMAT strftime output format"
      +> anon (maybe ("date" %: date))
    )
    (fun sdate edate fmt date () ->
      let selector = Sexp.input_sexp stdin |> Syntax.selector_of_sexp in
      match date with
        | None -> list_dates sdate edate selector fmt
        | Some d -> exit (if Filter.filter selector d sdate then 0 else 1)
    )

let () = Command.run command
