
let () =
  let green = "\027[92m" in
  let red = "\027[31m" in
  let stop = "\027[0m" in
  let pass = ref true in
  let test (name, func) =
    if func ()
      then prerr_endline (green ^ "Passed: " ^ name ^ stop)
      else (pass := false; prerr_endline (red ^ "Failed: " ^ name ^ stop)) in
  List.iter test Time.tests;
  print_newline ();
  if !pass
    then print_endline (green ^ "All tests have passed" ^ stop)
    else print_endline (red ^ "Some tests have failed" ^ stop)

