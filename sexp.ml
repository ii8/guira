
type sexp = Atom of string | List of sexp list

type state =
  | In_start
  | In_list of sexp list
  | In_atom of char list * sexp list
  | In_string of char list * sexp list

let atom cs =
  let len = List.length cs in
  let res = Bytes.create len in
  let rec imp i = function
    | [] -> res
    | c :: cs -> Bytes.set res i c; imp (i - 1) cs in
  Atom (imp (len - 1) cs)

let parse () =
  let next () = try Some (input_char stdin) with End_of_file -> None in
  let rec r state = match next () with
    | None -> failwith "unexpected eof"
    | Some c -> match state with
      | In_start -> begin match c with
        | ' ' | '\n' | '\t' -> r state
        | '(' -> r (In_list [])
        | _ -> failwith ("expressions must start with '('")
      end
      | In_list es -> begin match c with
        | ' ' | '\n' | '\t' -> r state
        | '(' -> r (In_list (r (In_list []) :: es))
        | ')' -> List (List.rev es)
        | '"' -> r (In_string ([], es))
        | _ -> r (In_atom ([c], es))
      end
      | In_atom (s, es) -> begin match c with
        | ' ' | '\n' | '\t' -> r (In_list (atom s :: es))
        | '(' -> r (In_list (r (In_list []) :: atom s :: es))
        | ')' -> List (List.rev (atom s :: es))
        | _ -> r (In_atom (c::s, es))
      end
      | In_string (s, es) -> begin match c with
        | '"' -> r (In_list (atom s :: es))
        | _ -> r (In_string (c::s, es))
      end in
  r In_start

let prerr exp =
  let rec p = function
    | Atom s -> s
    | List e -> "(" ^ (String.concat " " (List.map p e)) ^ ")" in
  prerr_endline(p exp)
