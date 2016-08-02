open Syntax

let f r p s d = try Filter.filter
  (Time.of_string r)
  (Time.interval_of_string p |> function None -> Time.Days | Some a -> a)
  s
  (Time.of_string d) with _ -> false

let _ = Callback.register "parse"
  (fun s -> try Sexp.parse_string s |> selector_of_sexp
   with _ -> Selector (Time.Years, Not All, []))

let _ = Callback.register "filter" f
