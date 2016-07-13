
let f s d r =
  Js.bool @@ Filter.filter
    (Js.to_string s |> Sexp.parse_string |> Syntax.selector_of_sexp)
    (Js.to_string d |> Time.of_string)
    (Js.to_string r |> Time.of_string)

let () = Js.export "guira" (Js.wrap_callback f)
