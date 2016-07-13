
let () =
  let green = "\027[92m" in
  let red = "\027[31m" in
  let stop = "\027[0m" in
  let pass = ref true in
  let test (name, func) =
    if func ()
      then prerr_endline (green ^ "Passed: " ^ name ^ stop)
      else (pass := false; prerr_endline (red ^ "Failed: " ^ name ^ stop)) in

  prerr_newline ();
  prerr_endline "|| Starting unit tests ||";
  List.iter test Sexp.tests;
  List.iter test Time.tests;
  prerr_newline ();
  if !pass
    then prerr_endline (green ^ "All unit tests have passed" ^ stop)
    else prerr_endline (red ^ "Some unit tests have failed" ^ stop)

